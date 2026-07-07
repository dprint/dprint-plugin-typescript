use deno_ast::ParseDiagnostic;
use deno_ast::ParseDiagnosticsError;

/// An error that occurred while formatting a file.
#[derive(Debug, thiserror::Error)]
pub enum FormatError {
  /// The source text could not be parsed due to a fatal syntax error.
  #[error(transparent)]
  Parse(#[from] ParseDiagnostic),
  /// The source text had one or more syntax errors that prevent formatting.
  #[error(transparent)]
  SyntaxErrors(#[from] ParseDiagnosticsError),
  /// Any other error that occurred while formatting (ex. a generation diagnostic).
  #[error("{0}")]
  Other(String),
}

impl From<String> for FormatError {
  fn from(message: String) -> Self {
    FormatError::Other(message)
  }
}

impl From<&str> for FormatError {
  fn from(message: &str) -> Self {
    FormatError::Other(message.to_string())
  }
}
