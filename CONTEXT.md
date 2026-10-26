# AMOS Harmonization

This context describes the reviewed evidence and terminology used to connect
AMOS methods to environmental materials and analytical procedures without
discarding source detail.

## Materials

**Analytical matrix**:
The material or sample analyzed by an AMOS method.
_Avoid_: Media category, sample type

**Matrix term**:
A reviewed ENVO term or local `AMH:` extension that represents one analytical
matrix.
_Avoid_: Route, raw matrix

**Matrix assertion**:
An evidence-backed link between one AMOS method and one matrix term. A method
covering several materials has several matrix assertions. Ontology ancestors
provide derived classification context for discovery and routing; they do not
assert that the method applies to every material classified under the ancestor.
_Avoid_: Matrix mapping

**Physical phase**:
The single dominant `liquid`, `solid`, or `gas` classification assigned to an
AMOS-mapped terminal matrix term.
_Avoid_: Unit route, media category

**CONCERT unit route**:
The optional compatibility route `aqueous`, `solid`, or `air` used for safe
unit conversion. Only water-based liquids may have the `aqueous` route.
_Avoid_: Physical phase

**Matrix qualifier**:
Context that narrows a matrix without creating another matrix assertion, such
as the textures in `soil (sand, sandy clay loam)`.
_Avoid_: Matrix

## Analytical stack

**Analytical technique**:
A procedure identified by a CHMO identifier and its full preferred label.
Abbreviations are aliases and retained source evidence, not canonical names.
_Avoid_: Methodology string, instrument model, abbreviation

**Analytical component**:
A sample preparation, separation, ionization, or detection step within an
analytical method.
_Avoid_: Composite method

**Analytical decomposition**:
A deterministic interpretation of one normalized reported phrase as an
overall analytical technique and/or its ordered analytical components. A
unique exact CHMO composite may establish the interpretation; otherwise it
requires review. The interpretation applies consistently wherever that phrase
occurs. Valid components remain canonical when CHMO has no term for their
composite.
_Avoid_: Delimiter split, inferred assertion

**Inclusive analytical match**:
A match in which the requested components occur in one analytical
decomposition in the requested order. Each component may match itself or a
CHMO descendant; other components may occur before, between, or after them.
_Avoid_: Exact phrase match

**Instrument class**:
A general CHMO device class, such as mass spectrometer.
_Avoid_: Instrument mention, model

**Instrument mention**:
Manufacturer or model text retained as evidence without becoming canonical.
_Avoid_: Instrument class

## Evidence and review

**Reported assertion**:
An assertion directly supplied by AMOS.
_Avoid_: Inferred assertion

**Auto assertion**:
A high-confidence assertion created by a deterministic rule that targets an
existing ontology node.
_Avoid_: Automatic node

**Reviewed assertion**:
An inference or local extension approved in a committed review decision.
_Avoid_: Auto assertion

**Review candidate**:
An unresolved or ambiguous phrase retained with its evidence for a person to
decide.
_Avoid_: Failed mapping

**Reference label**:
An expected interpretation for a validation item established through blind
independent dual review and adjudication.
_Avoid_: Gold label, model label

**Model-assisted review**:
Human review aided by machine-generated prioritization or term suggestions.
Its result is a reviewed assertion, not independent validation evidence.
_Avoid_: Auto assertion, reference label
