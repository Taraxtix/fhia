use ariadne::{Color, Fmt as _, Label as ALabel, Report, ReportKind, Source};
use chumsky::prelude::Rich;

use crate::diagnostics::{Diagnostic, Reportable, Severity};
use crate::lexer::Token;
use crate::parser::ParseOutput;

const KEYWORD: Color = Color::Magenta;

pub fn render(diagnostic: &Diagnostic, source: &str, source_name: &str) {
    let origin_span = diagnostic
        .labels
        .first()
        .map(|l| l.span.clone())
        .unwrap_or(0..0);

    let mut report = Report::build(
        match diagnostic.severity {
            Severity::Error => ReportKind::Error,
            // Severity::Warning => ReportKind::Warning,
            // Severity::Note => ReportKind::Advice,
        },
        (source_name, origin_span),
    )
    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
    .with_message(&diagnostic.message);

    if let Some(code) = diagnostic.code {
        report = report.with_code(code);
    }

    for (order, label) in diagnostic.labels.iter().enumerate() {
        report = report.with_label(
            ALabel::new((source_name, label.span.clone()))
                .with_message(label.message.clone())
                .with_color(match label.kind {
                    super::LabelKind::Main => Color::Red,
                    super::LabelKind::Context => Color::Cyan,
                })
                .with_order(order as i32),
        );
    }

    report
        .finish()
        .eprint((source_name, Source::from(source)))
        .unwrap();
}

pub fn from_chumsky(err: Rich<'_, Token<'_>>) -> Diagnostic {
    let mut span = err.span().into_range();

    if span.start == span.end && span.start > 0 {
        span.start -= 1;
    }

    let mut diagnostic = if err.found().is_some() {
        Diagnostic::error("Unexpected token")
    } else {
        Diagnostic::error("Unexpected end of input")
    };
    let found_msg: String;
    if let Some(found) = err.found() {
        found_msg = format!("{}", found.fg(KEYWORD));
    } else {
        found_msg = format!("{}", "EOF".fg(KEYWORD));
    }

    let mut expected: Vec<String> = err.expected().map(|e| e.to_string()).collect();
    expected.sort();
    expected.dedup();

    diagnostic = diagnostic.with_main_label(
        span,
        if expected.is_empty() {
            format!("{}. Found: {found_msg}", err.reason())
        } else if expected.len() == 1 {
            format!(
                "Expected: {}. But found: {found_msg}",
                expected[0].clone().fg(KEYWORD)
            )
        } else {
            format!(
                "Expected one of: {}. But found: {found_msg}",
                expected
                    .into_iter()
                    .map(|expected| expected.fg(KEYWORD).to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        },
    );

    const MAX_CONTEXT_LABELS: usize = 2;
    if let Some((ctx, ctx_span)) = err
        .contexts()
        .take(MAX_CONTEXT_LABELS)
        .map(|(pattern, span)| (pattern.to_string(), (*span).into_range()))
        .reduce(|acc, curr| {
            (
                if acc.0.is_empty() {
                    curr.0
                } else {
                    format! {"{} -> {}", acc.0, curr.0}
                },
                acc.1.start.min(curr.1.start)..acc.1.end.max(curr.1.end),
            )
        })
    {
        diagnostic = diagnostic.with_context_label(ctx_span, format!("Parsing: {ctx}"));
    }

    diagnostic
}

impl Reportable for ParseOutput<'_> {
    fn report(&self, source: &str, source_name: &str) {
        let mut should_exit = false;
        for diagnostic in &self.diagnostics {
            should_exit = diagnostic.severity == Severity::Error || should_exit;
            render(diagnostic, source, source_name);
        }
        if should_exit {
            std::process::exit(1);
        }
    }
}
