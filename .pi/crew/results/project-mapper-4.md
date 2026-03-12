# Project Mapping Results — noon

## Summary
Mapped the noon project: a Clojure/ClojureScript MIDI composition library built around a functional, composable update model where scores are plain sets of event maps.

## Files Produced (18 total)

### Top-level (6)
- `project.md` — identity, vitals, entry points, architecture brief
- `structure.md` — annotated directory tree
- `conventions.md` — code style, naming, patterns, anti-patterns
- `testing.md` — frameworks, commands, doc-test pipeline
- `dependencies.md` — all deps with roles (Clojure, npm, build)
- `learnings.md` — non-obvious discoveries (tagged types, SCI, doc-test pipeline)

### Modules (12)
score, events, updates, harmonic-context, eval, output, lib-harmony, lib-melody, lib-rythmn, parse-harmony, client, viz, utils

### Meta
- `changelog.md` — mapping session record

## Project Characterization
A well-structured functional composition library with a clean layered architecture: events → scores → updates → libs. The harmonic context module (1011 lines) is the theoretical core, implementing a multi-layer pitch system. SCI provides sandboxed evaluation for both JVM and browser.

## Confidence
- **High:** score, events, updates, eval (read thoroughly)
- **Medium:** harmonic-context (complex, read partially), lib/*, output, client, viz
- **Not mapped:** vst/* details, data/* internals, tries/* scratch files
