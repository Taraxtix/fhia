pub mod parser;

pub trait Reportable {
    fn report(&self, source: &str, source_name: &str);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(usize)]
pub enum ErrorCode {
    InvalidToken         = 0,
    UnexpectedToken      = 1,
    TopLevelNotDeclaration = 2,
    DeclarationMalformed = 3,
    DuplicateDeclaration = 4,
    UndefinedVariable    = 5,
    TypeAscriptionMismatch = 6,
    TypeMismatch         = 7,
    InvalidCastOperand   = 8,
}

impl ErrorCode {
    pub const fn title(self) -> &'static str {
        match self
        {
            Self::InvalidToken => "Invalid token",
            Self::UnexpectedToken => "Unexpected token",
            Self::TopLevelNotDeclaration => "Top level expressions must be declarations",
            Self::DeclarationMalformed => "Declaration malformed",
            Self::DuplicateDeclaration => "Duplicate declaration",
            Self::UndefinedVariable => "Undefined variable",
            Self::TypeAscriptionMismatch => "Type ascription mismatch",
            Self::TypeMismatch => "Type mismatch",
            Self::InvalidCastOperand => "Invalid cast operand",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    // Warning,
    // Note,
}

#[derive(Clone, Debug)]
pub enum LabelKind {
    Main,
    Context,
    // Hint,
}

#[derive(Clone, Debug)]
pub struct Label {
    pub span:    std::ops::Range<usize>,
    pub message: String,
    pub kind:    LabelKind,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code:     Option<u32>,
    pub message:  String,
    pub labels:   Vec<Label>,
}

impl Diagnostic {
    pub fn error(code: ErrorCode) -> Self {
        Self {
            severity: Severity::Error,
            code:     Some(code as u32),
            message:  code.title().to_string(),
            labels:   Vec::new(),
        }
    }

    #[must_use]
    pub fn with_main_label(
        mut self,
        span: std::ops::Range<usize>,
        message: impl Into<String>,
    ) -> Self {
        self.labels.push(Label {
            span,
            message: message.into(),
            kind: LabelKind::Main,
        });
        self
    }

    #[must_use]
    pub fn with_context_label(
        mut self,
        span: std::ops::Range<usize>,
        message: impl Into<String>,
    ) -> Self {
        self.labels.push(Label {
            span,
            message: message.into(),
            kind: LabelKind::Context,
        });
        self
    }
}
