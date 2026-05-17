use ariadne::{Color, Label as ALabel, Report, ReportKind, Source};

pub trait Reportable {
    fn diagnostics(&self) -> &[Diagnostic];
    fn report(&self, source: &str, source_name: &str) {
        let mut should_exit = false;
        for diagnostic in self.diagnostics()
        {
            should_exit = diagnostic.severity == Severity::Error || should_exit;
            render(diagnostic, source, source_name);
        }
        if should_exit
        {
            std::process::exit(1);
        }
    }
}

impl Reportable for Vec<Diagnostic> {
    fn diagnostics(&self) -> &[Diagnostic] { self.as_slice() }
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
    CyclicDeclaration    = 9,
    MissingMain          = 10,
    IncorrectMainType    = 11,
    IntLiteralOutOfRange = 12,
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
            Self::CyclicDeclaration => "Cyclic declaration",
            Self::MissingMain => "Missing main declaration",
            Self::IncorrectMainType => "Incorrect main type",
            Self::IntLiteralOutOfRange => "Integer literal out of range",
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

#[allow(clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
/// # Panics
/// - Panics if there is an error while printing the diagnostic to stderr
pub fn render(diagnostic: &Diagnostic, source: &str, source_name: &str) {
    let origin_span = diagnostic
        .labels
        .first()
        .map(|l| l.span.clone())
        .unwrap_or(0..0);

    let mut report = Report::build(
        match diagnostic.severity
        {
            Severity::Error => ReportKind::Error,
            // Severity::Warning => ReportKind::Warning,
            // Severity::Note => ReportKind::Advice,
        },
        (source_name, origin_span),
    )
    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
    .with_message(&diagnostic.message);

    if let Some(code) = diagnostic.code
    {
        report = report.with_code(code);
    }

    for (order, label) in diagnostic.labels.iter().enumerate()
    {
        report = report.with_label(
            ALabel::new((source_name, label.span.clone()))
                .with_message(label.message.clone())
                .with_color(match label.kind
                {
                    LabelKind::Main => Color::Red,
                    LabelKind::Context => Color::Cyan,
                })
                .with_order(order as i32),
        );
    }

    report
        .finish()
        .eprint((source_name, Source::from(source)))
        .expect("Failed to print to stderr");
}
