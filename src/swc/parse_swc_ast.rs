use std::path::Path;
use swc_common::{
    FileName, comments::SingleThreadedComments, SourceFile, BytePos, Spanned
};
use swc_ecmascript::parser::{Parser, StringInput, Syntax, error::{SyntaxError, Error as SwcError}, lexer::Lexer, Capturing, JscTarget, token::{TokenAndSpan}};

pub struct ParsedSourceFile {
    pub module: swc_ecmascript::ast::Module,
    pub info: SourceFile,
    pub tokens: Vec<TokenAndSpan>,
    pub comments: SingleThreadedComments,
}

pub fn parse_swc_ast(file_path: &Path, file_text: &str) -> Result<ParsedSourceFile, String> {
    match parse_inner(file_path, file_text) {
        Ok(result) => Ok(result),
        Err(err) => {
            if get_lowercase_extension(file_path) == Some(String::from("ts")) {
                // try to parse as jsx
                let tsx_file_path = file_path.with_extension("tsx");
                match parse_inner(&tsx_file_path, file_text) {
                    Ok(result) => Ok(result),
                    Err(_) => Err(err), // return the original error
                }
            } else {
                Err(err)
            }
        }
    }
}

fn parse_inner(file_path: &Path, file_text: &str) -> Result<ParsedSourceFile, String> {
    let source_file = SourceFile::new(
        FileName::Custom(file_path.to_string_lossy().into()),
        false,
        FileName::Custom(file_path.to_string_lossy().into()),
        file_text.into(),
        BytePos(0),
    );

    let comments: SingleThreadedComments = Default::default();
    let (module, tokens) = {
        let ts_config = swc_ecmascript::parser::TsConfig {
            tsx: should_parse_as_jsx(file_path),
            dynamic_import: true,
            decorators: true,
            import_assertions: true,
            dts: false,
            no_early_errors: false,
        };
        let lexer = Lexer::new(
            Syntax::Typescript(ts_config),
            JscTarget::Es2019,
            StringInput::from(&source_file),
            Some(&comments)
        );
        let lexer = Capturing::new(lexer);
        let mut parser = Parser::new_from(lexer);
        let parse_module_result = parser.parse_module();
        ensure_no_specific_syntax_errors(parser.take_errors(), file_text)?;
        let tokens = parser.input().take();

        match parse_module_result {
            Err(error) => Err(format_swc_error(error, file_text)),
            Ok(module) => Ok((module, tokens))
        }
    }?;

    // {
    //     let (leading, trailing) = comments.borrow_all();
    //     println!("Leading: {:?}", leading);
    //     println!("Trailing: {:?}", trailing);
    // }

    return Ok(ParsedSourceFile {
        comments,
        module,
        info: source_file,
        tokens,
    });

    fn should_parse_as_jsx(file_path: &Path) -> bool {
        if let Some(extension) = get_lowercase_extension(file_path) {
            return extension == "tsx" || extension == "jsx" || extension == "js" || extension == "mjs";
        }
        return true;
    }

    fn ensure_no_specific_syntax_errors(errors: Vec<SwcError>, file_text: &str) -> Result<(), String> {
        let errors = errors.into_iter().filter(|e| match e.clone().kind() { // todo: remove clone once PR #1396 lands in swc
            // expected identifier
            SyntaxError::TS1003 => true,
            // expected semi-colon
            SyntaxError::TS1005 => true,
            _ => false,
        }).collect::<Vec<_>>();

        if errors.is_empty() {
            Ok(())
        } else {
            let mut final_message = String::new();
            for error in errors {
                if !final_message.is_empty() {
                    final_message.push_str("\n\n");
                }
                final_message.push_str(&format_swc_error(error, file_text));
            }
            Err(final_message)
        }
    }
}

fn get_lowercase_extension(file_path: &Path) -> Option<String> {
    file_path.extension().and_then(|e| e.to_str()).map(|f| f.to_lowercase())
}

fn format_swc_error(error: SwcError, file_text: &str) -> String {
    let error_span = error.span();
    dprint_core::formatting::utils::string_utils::format_diagnostic(
        Some((error_span.lo().0 as usize, error_span.hi().0 as usize)),
        &error.kind().msg(),
        file_text
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn should_error_on_syntax_diagnostic() {
        let message = parse_swc_ast(&PathBuf::from("./test.ts"), "test;\nas#;").err().unwrap();
        assert_eq!(
            message,
            concat!(
                "Line 2, column 3: Expected ';', '}' or <eof>\n",
                "\n",
                "  as#;\n",
                "    ~"
            )
        );
    }

    #[test]
    fn it_should_error_for_no_equals_sign_in_var_decl() {
        let message = parse_swc_ast(&PathBuf::from("./test.ts"), "const Methods {\nf: (x, y) => x + y,\n};").err().unwrap();
        assert_eq!(
            message,
            concat!(
                "Line 1, column 15: Expected a semicolon\n",
                "\n",
                "  const Methods {\n",
                "                ~"
            )
        );
    }

    #[test]
    fn it_should_error_when_var_stmts_sep_by_comma() {
        let message = parse_swc_ast(&PathBuf::from("./test.ts"), "let a = 0, let b = 1;").err().unwrap();
        assert_eq!(
            message,
            concat!(
                "Line 1, column 16: Expected a semicolon\n",
                "\n",
                "  let a = 0, let b = 1;\n",
                "                 ~"
            )
        );
    }

    #[test]
    fn it_should_error_without_issue_when_there_exists_multi_byte_char_on_line_with_syntax_error() {
        let message = parse_swc_ast(
            &PathBuf::from("./test.ts"),
            concat!(
                "test;\n",
                r#"console.log("x", `duration ${d} not in range - ${min} ≥ ${d} && ${max} ≥ ${d}`),;"#,
            )
        ).err().unwrap();
        assert_eq!(
            message,
            concat!(
                // quite the diagnostic!
                "Line 2, column 81: Unexpected token `;`. Expected this, import, async, function, [ for array literal, ",
                "{ for object literal, @ for decorator, function, class, null, true, false, number, bigint, string, ",
                "regexp, ` for template literal, (, or an identifier\n",
                "\n",
                "  & ${max} ≥ ${d}`),;\n",
                "                    ~"
            )
        );
    }
}
