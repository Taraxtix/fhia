pub mod parser;

pub trait Reportable {
    fn report(&self, source: &str, source_name: &str);
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
    pub span: std::ops::Range<usize>,
    pub message: String,
    pub kind: LabelKind,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<u32>,
    pub message: String,
    pub labels: Vec<Label>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            code: None,
            message: message.into(),
            labels: Vec::new(),
        }
    }

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

    pub fn with_context_label(mut self, span: std::ops::Range<usize>, message: impl Into<String>) -> Self {
        self.labels.push(Label {
            span,
            message: message.into(),
            kind: LabelKind::Context,
        });
        self
    }

}
