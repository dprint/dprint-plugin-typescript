use std::cell::OnceCell;
use std::sync::Arc;

use dprint_swc_ext::common::SourcePos;
use dprint_swc_ext::common::SourceRange;
use dprint_swc_ext::common::SourceRanged;
use dprint_swc_ext::common::SourceTextInfo;
use dprint_swc_ext::common::SourceTextProvider;
use dprint_swc_ext::common::StartSourcePos;
use dprint_swc_ext::swc::ast::Program;
use dprint_swc_ext::swc::common::comments::SingleThreadedComments;
use dprint_swc_ext::swc::common::comments::SingleThreadedCommentsMapInner;
use dprint_swc_ext::swc::parser::token::TokenAndSpan;
use dprint_swc_ext::swc::parser::Syntax;
use dprint_swc_ext::view::Comments;
use dprint_swc_ext::view::ProgramInfo;
use dprint_swc_ext::view::ProgramInfoProvider;
use dprint_swc_ext::view::ProgramRef;

use super::ParseDiagnostic;

/// A source parsed by this crate, containing an AST, comments, and tokens.
///
/// Implements [`ProgramInfoProvider`], so it can be handed to
/// [`format_parsed_source`](crate::format_parsed_source).
///
/// This is `Send` but not `Sync`, so it can be parsed on one thread and
/// formatted on another, but not shared between threads by reference.
pub struct ParsedSource {
  pub(super) specifier: Arc<str>,
  pub(super) text: Arc<str>,
  pub(super) syntax: Syntax,
  pub(super) text_info: OnceCell<SourceTextInfo>,
  pub(super) program: Program,
  pub(super) comments: ParsedComments,
  pub(super) tokens: Vec<TokenAndSpan>,
  pub(super) diagnostics: Vec<ParseDiagnostic>,
}

impl ParsedSource {
  /// Specifier of the source that was parsed.
  pub fn specifier(&self) -> &str {
    &self.specifier
  }

  /// Text of the source that was parsed, with any byte order mark stripped.
  pub fn text(&self) -> &Arc<str> {
    &self.text
  }

  /// Syntax the source was parsed with, which is what the formatter needs in
  /// order to know whether some syntax would be ambiguous.
  pub fn syntax(&self) -> Syntax {
    self.syntax
  }

  /// Gets an object with pre-computed positions for lines and indexes of
  /// multi-byte chars.
  ///
  /// Note: Prefer using `.text()` over this if able because this is lazily
  /// created.
  pub fn text_info(&self) -> &SourceTextInfo {
    self.text_info.get_or_init(|| SourceTextInfo::new(self.text.clone()))
  }

  /// Range of the parsed source.
  pub fn range(&self) -> SourceRange<StartSourcePos> {
    SourceRange::new(StartSourcePos::START_SOURCE_POS, StartSourcePos::START_SOURCE_POS + self.text.len())
  }

  /// Gets the parsed program.
  pub fn program(&self) -> &Program {
    &self.program
  }

  /// Gets the comments found in the source.
  pub fn comments(&self) -> &ParsedComments {
    &self.comments
  }

  /// Gets the tokens found in the source.
  pub fn tokens(&self) -> &[TokenAndSpan] {
    &self.tokens
  }

  /// Gets the non-fatal syntax errors found while parsing.
  ///
  /// swc recovers from these, so the AST may not represent the original text.
  /// See [`is_unsupported_syntax_error`](crate::is_unsupported_syntax_error).
  pub fn diagnostics(&self) -> &[ParseDiagnostic] {
    &self.diagnostics
  }
}

impl ProgramInfoProvider for ParsedSource {
  fn program_info(&self) -> ProgramInfo<'_> {
    ProgramInfo {
      program: match &self.program {
        Program::Module(module) => ProgramRef::Module(module),
        Program::Script(script) => ProgramRef::Script(script),
      },
      text_info: Some(self.text_info()),
      tokens: Some(&self.tokens),
      comments: Some(Comments {
        leading: &self.comments.leading,
        trailing: &self.comments.trailing,
      }),
    }
  }
}

impl<'a> SourceTextProvider<'a> for &'a ParsedSource {
  fn text(&self) -> &'a Arc<str> {
    ParsedSource::text(self)
  }

  fn start_pos(&self) -> StartSourcePos {
    StartSourcePos::START_SOURCE_POS
  }
}

impl SourceRanged for ParsedSource {
  fn start(&self) -> SourcePos {
    StartSourcePos::START_SOURCE_POS.as_source_pos()
  }

  fn end(&self) -> SourcePos {
    StartSourcePos::START_SOURCE_POS + self.text.len()
  }
}

impl std::fmt::Debug for ParsedSource {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    f.debug_struct("ParsedSource")
      .field("specifier", &self.specifier)
      .field("program", &self.program)
      .finish()
  }
}

/// The leading and trailing comments found while parsing.
#[derive(Debug, Default)]
pub struct ParsedComments {
  pub(super) leading: SingleThreadedCommentsMapInner,
  pub(super) trailing: SingleThreadedCommentsMapInner,
}

impl ParsedComments {
  pub(super) fn from_single_threaded(comments: SingleThreadedComments) -> Self {
    fn take(map: std::rc::Rc<std::cell::RefCell<SingleThreadedCommentsMapInner>>) -> SingleThreadedCommentsMapInner {
      match std::rc::Rc::try_unwrap(map) {
        Ok(cell) => cell.into_inner(),
        // only possible if the comments were cloned, which they aren't here
        Err(rc) => rc.borrow().clone(),
      }
    }

    let (leading, trailing) = comments.take_all();
    Self {
      leading: take(leading),
      trailing: take(trailing),
    }
  }

  /// Map of comments that appear before a position.
  pub fn leading_map(&self) -> &SingleThreadedCommentsMapInner {
    &self.leading
  }

  /// Map of comments that appear after a position.
  pub fn trailing_map(&self) -> &SingleThreadedCommentsMapInner {
    &self.trailing
  }
}
