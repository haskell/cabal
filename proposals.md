# Cabal Proposals Process

## Motivation

The Cabal project's codebase is large, complex, and has evolved over many years. No single
person fully understands how all its parts interact. As a result, maintainers
and contributors often hesitate to make significant changes, unsure whether they
are safe or whether they align with the project's long-term direction. This
has led to a delayed decision making process and a tendency to avoid
making significant decisions. One example is the removal of legacy v1- commands: although most developers agree they should be deprecated and eventually removed, no one feels confident in making that change without a clearer mandate or process.

To address this, we propose a
lightweight, developer-led process for making and recording significant
decisions about the future of Cabal and cabal-install. The goal is to enable
contributors to move the project forward with greater confidence and
shared understanding.

The process is designed to make developers feel empowered to make decisions.

* It is light-weight, a PR is opened and discussed on a repo with a fixed discussion period.
* It is developer-led, final decisions are made by developers at the Cabal developers meeting.
* It is flexible, there is no formal voting procedure, decisions are made by [rough consensus](https://datatracker.ietf.org/doc/html/rfc7282).

Overall, we hope that this will allow developers to move the cabal project forward.

This process is an experiment. We will review it over time to ensure that it continues to serve the Cabal project well.

## Principles

- The process exists to support active contributors, not to control them.
- The decision-making power remains with those who contribute code and maintain the project.
- The process is deliberately kept lightweight to avoid burdening contributors.
  If contributors do not want to use the process then it should be modified.
- The developers meeting will assess consensus to make a final decisions.
- Transparency and documentation make it easier for all contributors to follow significant decisions.


## Scope

This process applies to:

- Changes to the `Cabal` library.
- Changes to the `cabal-install` tool.
- Significant process or tooling changes for the Cabal project itself.

This process does **not** apply to:

- The broader Haskell ecosystem.
- Community-wide standards.
- Individual packages not maintained in the Cabal project.

Routine or small changes do not require a formal proposal; they can continue to be discussed and merged via the normal PR process.

### Examples

* Bug fixes or small improvements do not require proposals.
* Improving features which already see wide agreement does not require proposals (for example, migrating existing commands from v1- to v2-).
* Larger features should be first discussed on a proposal (for example, a new command, Hooks, private dependencies).
* Significant changes to existing behaviour should be discussed on proposals.

In general, most changes do not require proposals, developers are trusted to use their judgement about when seeking a broader consensus is necessary.

## Process

### 1. Proposal Submission

- Proposals are submitted as **pull requests** to the [`cabal-proposals`](https://github.com/haskell/cabal-proposals) repository.
- The proposal must follow the [proposal template](#proposal-template).
- Anyone can submit a proposal. However, proposals without the support of active contributors are unlikely to be accepted.
- There is a general expectation that authors of proposals will take the lead on their implementation. If proposals remain unimplemented for an extended period, it may affect how future proposals from the same author are received.

### 2. Discussion

- Discussion happens on the pull request.
- There is a **minimum comment period** of 2 weeks, or until the next Cabal developers meeting (whichever is longer). Extensions may be granted for more complex topics.
- All community members are welcome to comment. Maintainers and active contributors are especially encouraged to provide feedback.
- After the minimum comment period, the author can request for the proposal to be decided on at
  the Cabal developers meeting.

### 3. Decision Meeting

- The Cabal developers meeting is the forum for making decisions on proposals.
- The developers present at the meeting should reflect on the comments on a proposal and determine the [rough consensus](https://datatracker.ietf.org/doc/html/rfc7282) of the community.
  The opinion of knowledgeable contributors regarding a particular subsystem is especially
  important for the meeting to reach a decision.
- It is not necessarily expected that the participants of the meeting will offer a technical opinion. The discussion on the issue should provide enough context for a decision to be made.
- There is a quorum of three developers at the meeting.
- If quorum is not reached for 4 consecutive meetings (8 weeks) whilst a proposal is due to be discussed, the proposal process is suspended and reviewed.

## 4. The Decision

- A proposal is **accepted** if there is general agreement among active maintainers and contributors present at the meeting.
- A proposal is **rejected** if consensus cannot be reached or the developers do not wish to
  pursue the direction of the proposal.
- A proposal is **deferred** if the developers generally agree with the direction of the proposal
  but have specific and actionable points which require improvement before acceptance.
  For example, the opinion of a certain expert may be additionally sought or they may wish for more consideration into the backwards compatibility story.
- The meeting chair is responsible for recording the outcome on the proposal PR.


### 5. Outcome and Implementation

- **Accepted** proposals are merged into the `cabal-proposals` repository and tagged as "Accepted".
- **Rejected** proposals are closed with a clear rationale.
- **Deferred** proposals may be left open with an explicit milestone for revisiting.

Acceptance of a proposal does not automatically result in implementation. The proposer is expected to implement the change or coordinate with others to do so.

### Example Timeline

* A proposal is opened.
* A two weeks discussion period follows.
* After at least two weeks, the author may ask cabal developers to consider their proposal.
* The cabal developers may decide about the proposal at the next developers' meeting or a cabal developer may ask for a longer discussion period.
* The proposal is accepted: the proposal author implements the feature.
* The proposal is rejected: the PR is closed.
* The proposal is deferred: the decision period is extended and a concrete
  reason for deferral is named. Once the concern has been addressed then the
  proposal is reviewed again at the meeting.


## Proposal Template

Each proposal should include the following sections:

```markdown
# Title

## Summary

A short, high-level summary of the proposed change.

## Motivation

Why is this change needed? What problem does it solve?

## Proposed Change

Describe the change in detail.

## Alternatives Considered

What other approaches were considered? Why was this one chosen?

## Backwards Compatibility / Migration

Does this change affect backwards compatibility? What migration path is needed?

## Interested parties

Who are the interested parties in the broader Haskell community? Have you contacted them?

## Implementation Notes

Are you willing to implement this yourself? What is the expected timeline?

## Open Questions

Are there any unresolved questions or areas needing further input?

## References

Links to related issues, discussions, or previous work.
```
