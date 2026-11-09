use std::sync::Arc;

use oxc_allocator::Allocator;
use oxc_ast::ast::Program;
use oxc_parser::Token;
use oxc_span::SourceType;

use super::ParseDiagnostic;
use crate::ast::SourceTextInfo;

/// A source parsed by this crate, containing an AST, comments, and tokens.
///
/// It can be handed to [`format_parsed_source`](crate::format_parsed_source).
pub struct ParsedSource {
  pub(super) specifier: Arc<str>,
  pub(super) source_type: SourceType,
  pub(super) cell: ParsedSourceCell,
  pub(super) diagnostics: Vec<ParseDiagnostic>,
}

pub(super) struct ParsedSourceOwner {
  pub allocator: Allocator,
  pub text: Arc<str>,
}

pub(super) struct ParsedProgram<'a> {
  pub program: Program<'a>,
  pub tokens: oxc_allocator::Vec<'a, Token>,
}

self_cell::self_cell!(
  pub(super) struct ParsedSourceCell {
    owner: ParsedSourceOwner,
    #[not_covariant]
    dependent: ParsedProgram,
  }
);

impl ParsedSource {
  /// Specifier of the source that was parsed.
  pub fn specifier(&self) -> &str {
    &self.specifier
  }

  /// Text of the source that was parsed, with any byte order mark stripped.
  pub fn text(&self) -> &Arc<str> {
    &self.cell.borrow_owner().text
  }

  /// Source type the source was parsed with.
  pub fn source_type(&self) -> SourceType {
    self.source_type
  }

  /// Creates an object with pre-computed positions for lines.
  pub fn text_info(&self) -> SourceTextInfo {
    SourceTextInfo::new(self.text().clone())
  }

  /// Provides the parsed program and its tokens to the closure.
  pub fn with_program<R>(&self, f: impl for<'a> FnOnce(&'a Program<'a>, &'a [Token]) -> R) -> R {
    self.cell.with_dependent(|_, parsed| f(&parsed.program, &parsed.tokens))
  }

  /// Gets the non-fatal syntax errors found while parsing.
  ///
  /// The parser recovered from these, so the AST still represents the original text.
  pub fn diagnostics(&self) -> &[ParseDiagnostic] {
    &self.diagnostics
  }
}

impl std::fmt::Debug for ParsedSource {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    f.debug_struct("ParsedSource").field("specifier", &self.specifier).finish()
  }
}
