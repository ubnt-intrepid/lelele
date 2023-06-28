use anyhow::Context;
use lelele_example_min_caml::{lexer::lexer, parser::ParserDef};
use lelele_runtime::parser::{ParseEvent, Parser};
use std::env;

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_ansi(true)
        .with_max_level(tracing::Level::TRACE)
        .init();

    let input = env::args().nth(1).context("missing input")?;
    let mut tokens = lexer(&input);
    let mut parser = Parser::new(ParserDef::default());

    loop {
        let event = parser.resume().map_err(|e| {
            eprintln!("parse error: {:?}", e);
            e
        })?;
        match event {
            ParseEvent::InputNeeded => match tokens.next() {
                Some(tok) => {
                    let tok = tok?;
                    tracing::trace!("offer token {:?}", tok);
                    parser.offer_token(tok)?;
                }
                None => {
                    tracing::trace!("offer end of input");
                    parser.offer_eoi()?;
                }
            },

            ParseEvent::Shifting(lookahead) => {
                tracing::trace!("shifting: lookahead = {:?}", lookahead);
            }

            ParseEvent::AboutToReduce(reduce, args) => {
                tracing::trace!("about to reduce({:?}, {:?})", reduce, args);
            }

            ParseEvent::AboutToAccept(..) => {
                tracing::trace!("about to accept");
            }

            ParseEvent::HandlingError {
                lr_state,
                lookahead,
                expected,
            } => {
                tracing::trace!(
                    "handling error: lr_state={:?}, lookahead={:?}, expected={:?}",
                    lr_state,
                    lookahead,
                    expected,
                );

                continue;
            }

            ParseEvent::Accepted => {
                tracing::trace!("accepted");
                break;
            }

            ParseEvent::Rejected => {
                tracing::trace!("rejected");
                anyhow::bail!("rejected");
            }
        }
    }

    Ok(())
}
