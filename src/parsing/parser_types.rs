use dprint_core::formatting::{Info, ConditionReference};
use swc_common::SourceFile;
use swc_ast_view::*;
use fnv::{FnvHashMap, FnvHashSet};

use super::*;
use crate::configuration::*;
use crate::utils::Stack;

pub struct Context<'a> {
    pub module: &'a Module<'a>,
    pub config: &'a Configuration,
    pub comments: CommentTracker<'a>,
    pub token_finder: TokenFinder<'a>,
    pub current_node: Node<'a>,
    pub parent_stack: Stack<Node<'a>>,
    handled_comments: FnvHashSet<BytePos>,
    pub info: &'a SourceFile,
    stored_infos: FnvHashMap<(BytePos, BytePos), Info>,
    stored_info_ranges: FnvHashMap<(BytePos, BytePos), (Info, Info)>,
    pub end_statement_or_member_infos: Stack<Info>,
    before_comments_start_info_stack: Stack<(Span, Info)>,
    if_stmt_last_brace_condition_ref: Option<ConditionReference>,
    /// Used for ensuring nodes are parsed in order.
    #[cfg(debug_assertions)]
    pub last_parsed_node_pos: u32,
}

impl<'a> Context<'a> {
    pub fn new(
        config: &'a Configuration,
        tokens: &'a Vec<TokenAndSpan>,
        current_node: Node<'a>,
        info: &'a SourceFile,
        module: &'a Module,
    ) -> Context<'a> {
        Context {
            module,
            config,
            comments: CommentTracker::new(module, tokens),
            token_finder: TokenFinder::new(module),
            current_node,
            parent_stack: Stack::new(),
            handled_comments: FnvHashSet::default(),
            info,
            stored_infos: FnvHashMap::default(),
            stored_info_ranges: FnvHashMap::default(),
            end_statement_or_member_infos: Stack::new(),
            before_comments_start_info_stack: Stack::new(),
            if_stmt_last_brace_condition_ref: None,
            #[cfg(debug_assertions)]
            last_parsed_node_pos: 0,
        }
    }

    pub fn parent(&self) -> &Node<'a> {
        self.parent_stack.peek().unwrap()
    }

    pub fn has_handled_comment(&self, comment: &Comment) -> bool {
        self.handled_comments.contains(&comment.lo())
    }

    pub fn mark_comment_handled(&mut self, comment: &Comment) {
        self.handled_comments.insert(comment.lo());
    }

    pub fn store_info_for_node(&mut self, node: &dyn Spanned, info: Info) {
        self.stored_infos.insert((node.lo(), node.hi()), info);
    }

    pub fn get_info_for_node(&self, node: &dyn Spanned) -> Option<Info> {
        self.stored_infos.get(&(node.lo(), node.hi())).map(|x| x.to_owned())
    }

    pub fn store_info_range_for_node(&mut self, node: &dyn Spanned, infos: (Info, Info)) {
        self.stored_info_ranges.insert((node.lo(), node.hi()), infos);
    }

    pub fn get_info_range_for_node(&self, node: &dyn Spanned) -> Option<(Info, Info)> {
        self.stored_info_ranges.get(&(node.lo(), node.hi())).map(|x| x.to_owned())
    }

    pub fn store_if_stmt_last_brace_condition_ref(&mut self, condition_reference: ConditionReference) {
        self.if_stmt_last_brace_condition_ref = Some(condition_reference);
    }

    pub fn take_if_stmt_last_brace_condition_ref(&mut self) -> Option<ConditionReference> {
        self.if_stmt_last_brace_condition_ref.take()
    }

    pub fn get_or_create_current_before_comments_start_info(&mut self) -> Info {
        let current_span = self.current_node.span();
        if let Some((span, info)) = self.before_comments_start_info_stack.peek() {
            if *span == current_span {
                return *info;
            }
        }

        let new_info = Info::new("beforeComments");
        self.before_comments_start_info_stack.push((current_span, new_info));
        new_info
    }

    pub fn take_current_before_comments_start_info(&mut self) -> Option<Info> {
        let mut had_span = false;
        if let Some((span, _)) = self.before_comments_start_info_stack.peek() {
            if *span == self.current_node.span() {
                had_span = true;
            }
        }

        if had_span {
            Some(self.before_comments_start_info_stack.pop().1)
        } else {
            None
        }
    }

    // do any assertions for how the state of this context should be at the end of the file
    #[cfg(debug_assertions)]
    pub fn assert_end_of_file_state(&self) {
        if self.before_comments_start_info_stack.iter().next().is_some() {
            panic!("Debug panic! There were infos in the before comments start info stack.");
        }
    }

    #[cfg(debug_assertions)]
    pub fn assert_text(&self, span: Span, expected_text: &str) {
        let actual_text = span.text_fast(self.module);
        if actual_text != expected_text {
            panic!("Debug Panic Expected text `{}`, but found `{}`", expected_text, actual_text)
        }
    }
}

pub trait SpannedExtensions {
    fn start_line_with_comments(&self, context: &mut Context) -> usize;
    fn end_line_with_comments(&self, context: &mut Context) -> usize;
}

impl<T> SpannedExtensions for T where T : Spanned {
    /// Gets the start line with possible first leading comment start.
    fn start_line_with_comments(&self, context: &mut Context) -> usize {
        // The start position with comments is the next non-whitespace position
        // after the previous token's trailing comments. The trailing comments
        // are similar to the Roslyn definition where it's any comments on the
        // same line or a single multi-line block comment that begins on the trailing line.
        let mut leading_comments = self.leading_comments_fast(context.module);
        if leading_comments.is_empty() {
            self.start_line_fast(context.module)
        } else {
            let lo = self.lo();
            let previous_token = context.token_finder.get_previous_token(&lo);
            if let Some(previous_token) = previous_token {
                let previous_end_line = previous_token.end_line_fast(context.module);
                let mut past_trailing_comments = false;
                for comment in leading_comments {
                    let comment_start_line = comment.start_line_fast(context.module);
                    if !past_trailing_comments && comment_start_line <= previous_end_line {
                        let comment_end_line = comment.end_line_fast(context.module);
                        if comment_end_line > previous_end_line {
                            past_trailing_comments = true;
                        }
                    } else {
                        return comment_start_line;
                    }
                }

                self.start_line_fast(context.module)
            } else {
                leading_comments.next().unwrap().start_line_fast(context.module)
            }
        }
    }

    /// Gets the end line with possible trailing multi-line block comment end.
    fn end_line_with_comments(&self, context: &mut Context) -> usize {
        // start searching from after the trailing comma if it exists
        let search_end = context.token_finder.get_next_token_if_comma(self).map(|x| x.hi()).unwrap_or(self.hi());
        let trailing_comments = search_end.trailing_comments_fast(context.module);
        let previous_end_line = search_end.end_line_fast(context.module);
        for comment in trailing_comments {
            // optimization
            if comment.kind == CommentKind::Line { break; }

            let comment_start_line = comment.start_line_fast(context.module);
            if comment_start_line <= previous_end_line {
                let comment_end_line = comment.end_line_fast(context.module);
                if comment_end_line > previous_end_line {
                    return comment_end_line; // should only include the first multi-line comment block
                }
            } else {
                break;
            }
        }

        previous_end_line
    }
}

/* custom enums */

pub enum TypeParamNode<'a> {
    Instantiation(&'a TsTypeParamInstantiation<'a>),
    Decl(&'a TsTypeParamDecl<'a>)
}

impl<'a> Spanned for TypeParamNode<'a> {
    fn span(&self) -> Span {
        match self {
            TypeParamNode::Instantiation(node) => node.span(),
            TypeParamNode::Decl(node) => node.span(),
        }
    }
}

impl<'a> TypeParamNode<'a> {
    pub fn params(&self) -> Vec<Node<'a>> {
        match self {
            TypeParamNode::Instantiation(node) => node.params.iter().map(|p| p.into()).collect(),
            TypeParamNode::Decl(node) => node.params.iter().map(|&p| p.into()).collect(),
        }
    }

    pub fn parent(&self) -> Node<'a> {
        match self {
            TypeParamNode::Instantiation(node) => node.parent().into(),
            TypeParamNode::Decl(node) => node.parent().into(),
        }
    }
}

/* InnerSpanned */

pub trait InnerSpanned {
    fn get_inner_span(&self, context: &mut Context) -> Span;
}

impl<'a> InnerSpanned for &BlockStmt<'a> {
    fn get_inner_span(&self, _: &mut Context) -> Span {
        get_inner_span_for_object_like(&self.span())
    }
}

impl<'a> InnerSpanned for &ObjectLit<'a> {
    fn get_inner_span(&self, _: &mut Context) -> Span {
        get_inner_span_for_object_like(&self.span())
    }
}

impl<'a> InnerSpanned for &ObjectPat<'a> {
    fn get_inner_span(&self, _: &mut Context) -> Span {
        get_inner_span_for_object_like(&self.span())
    }
}

fn get_inner_span_for_object_like(span: &Span) -> Span {
    Span {
        lo: BytePos(span.lo.0 + 1),
        hi: BytePos(span.hi.0 - 1),
        ctxt: Default::default()
    }
}

pub trait NodeExtensions<'a> {
    fn get_type_parameters(&self) -> &Option<&'a TsTypeParamDecl>;
}

impl<'a> NodeExtensions<'a> for Node<'a> {
    fn get_type_parameters(&self) -> &Option<&'a TsTypeParamDecl> {
        match self {
            Node::ClassDecl(node) => &node.class.type_params,
            Node::Class(node) => &node.type_params,
            Node::TsInterfaceDecl(node) => &node.type_params,
            Node::ClassExpr(node) => &node.class.type_params,
            Node::FnDecl(node) => &node.function.type_params,
            Node::Function(node) => &node.type_params,
            Node::ClassMethod(node) => &node.function.type_params,
            Node::TsTypeAliasDecl(node) => &node.type_params,
            Node::ArrowExpr(node) => &node.type_params,
            Node::TsCallSignatureDecl(node) => &node.type_params,
            Node::TsConstructSignatureDecl(node) => &node.type_params,
            Node::TsMethodSignature(node) => &node.type_params,
            Node::MethodProp(node) => &node.function.type_params,
            Node::TsConstructorType(node) => &node.type_params,
            Node::TsFnType(node) => &node.type_params,
            _ => &None
        }
    }
}

/* ParametersSpanned */

pub trait ParametersSpanned {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span>;
}

impl<'a> ParametersSpanned for &Function<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.params.iter().map(|x| x.span()).collect(),
            self.return_type.map(|t| t.lo())
                .or(self.body.as_ref().map(|b| b.lo()))
                .unwrap_or(self.hi()),
            context
        )
    }
}

impl<'a> ParametersSpanned for &PrivateMethod<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        self.function.get_parameters_span(context)
    }
}

impl<'a> ParametersSpanned for &ClassMethod<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        self.function.get_parameters_span(context)
    }
}

impl<'a> ParametersSpanned for &Constructor<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.params.iter().map(|x| x.span()).collect(),
            self.body.as_ref().map(|t| t.lo())
                .unwrap_or(self.hi()),
            context
        )
    }
}

impl<'a> ParametersSpanned for &MethodProp<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        self.function.get_parameters_span(context)
    }
}

impl<'a> ParametersSpanned for &GetterProp<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            vec![],
            self.type_ann.as_ref().map(|t| t.lo())
                .or(self.body.as_ref().map(|t| t.lo()))
                .unwrap_or(self.hi()),
            context
        )
    }
}

impl<'a> ParametersSpanned for &SetterProp<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            vec![self.param.span()],
            self.body.as_ref().map(|t| t.lo()).unwrap_or(self.hi()),
            context
        )
    }
}

impl<'a> ParametersSpanned for &ArrowExpr<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.params.iter().map(|x| x.span()).collect(),
            self.return_type.as_ref().map(|t| t.lo()).unwrap_or(self.body.lo()),
            context
        )
    }
}

impl<'a> ParametersSpanned for &CallExpr<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.args.iter().map(|a| a.span()).collect(),
            self.hi(),
            context
        )
    }
}

impl<'a> ParametersSpanned for &NewExpr<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.args.as_ref().map(|args| args.iter().map(|a| a.span()).collect()).unwrap_or_default(),
            self.hi(),
            context
        )
    }
}

impl<'a> ParametersSpanned for &TsCallSignatureDecl<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.params.iter().map(|x| x.span()).collect(),
            self.type_ann.as_ref().map(|t| t.lo()).unwrap_or(self.hi()),
            context
        )
    }
}

impl<'a> ParametersSpanned for &TsConstructSignatureDecl<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.params.iter().map(|x| x.span()).collect(),
            self.type_ann.as_ref().map(|t| t.lo()).unwrap_or(self.hi()),
            context
        )
    }
}

impl<'a> ParametersSpanned for &TsGetterSignature<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            Vec::with_capacity(0),
            self.type_ann.as_ref().map(|t| t.lo()).unwrap_or(self.hi()),
            context
        )
    }
}

impl<'a> ParametersSpanned for &TsSetterSignature<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            vec![self.param.span()],
            self.hi(),
            context
        )
    }
}

impl<'a> ParametersSpanned for &TsMethodSignature<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.params.iter().map(|x| x.span()).collect(),
            self.type_ann.as_ref().map(|t| t.lo()).unwrap_or(self.hi()),
            context
        )
    }
}

impl<'a> ParametersSpanned for &TsConstructorType<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.params.iter().map(|x| x.span()).collect(),
            self.type_ann.lo(),
            context
        )
    }
}

impl<'a> ParametersSpanned for &TsFnType<'a> {
    fn get_parameters_span(&self, context: &mut Context) -> Option<Span> {
        get_params_or_args_span(
            self.lo(),
            self.params.iter().map(|x| x.span()).collect(),
            self.type_ann.lo(),
            context
        )
    }
}

fn get_params_or_args_span(start_pos: BytePos, params: Vec<Span>, following_pos: BytePos, context: &mut Context) -> Option<Span> {
    let close_token_end = {
        let close_paren = if let Some(last_param) = params.last() {
            context.token_finder.get_first_close_paren_after(last_param)
        } else {
            context.token_finder.get_first_close_paren_before(&following_pos)
        };
        if let Some(close_paren) = close_paren {
            let end = close_paren.hi();
            if end > start_pos {
                Some(end)
            } else {
                None
            }
        } else {
            None
        }
    }?;
    let close_token_start = {
        let open_paren = context.token_finder.get_first_open_paren_before(&{
            if let Some(first_param) = params.first() {
                first_param.lo()
            } else {
                close_token_end
            }
        });

        if let Some(open_paren) = open_paren {
            let pos = open_paren.lo();
            if pos >= start_pos {
                Some(pos)
            } else {
                None
            }
        } else {
            None
        }
    }?;

    Some(Span {
        lo: close_token_start,
        hi: close_token_end,
        ctxt: Default::default(),
    })
}
