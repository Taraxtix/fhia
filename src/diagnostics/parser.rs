use ariadne::{Color, Label as ALabel, Report, ReportKind, Source};

use crate::diagnostics::{Diagnostic, Reportable, Severity};
use crate::parser::ParseOutput;
use crate::typer::TypedOutput;

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
                    super::LabelKind::Main => Color::Red,
                    super::LabelKind::Context => Color::Cyan,
                })
                .with_order(order as i32),
        );
    }

    report
        .finish()
        .eprint((source_name, Source::from(source)))
        .expect("Failed to print to stderr");
}

impl Reportable for ParseOutput<'_> {
    fn report(&self, source: &str, source_name: &str) {
        let mut should_exit = false;
        for diagnostic in &self.diagnostics
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

impl Reportable for TypedOutput<'_> {
    fn report(&self, source: &str, source_name: &str) {
        let mut should_exit = false;
        for diagnostic in &self.diagnostics
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
