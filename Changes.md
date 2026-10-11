# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- `uploadGallery` builds `app.bsky.embed.gallery` embeds, and `createPost` accepts `embed => { gallery => [ ... ] }`. Each item supports `image`, `alt`, and `aspectRatio`.
- `videoEmbed( $blob, %opts )` wraps an already-uploaded video blob (such as the processed blob from `getVideoJobStatus`) as an `app.bsky.embed.video` with `alt`, `aspectRatio`, `presentation`, and `captions`.
- `uploadImages` and `uploadGallery` now pass through a per-image `aspectRatio`; `uploadVideo` now accepts `presentation`.
- `createPost` accepts a fully-built embed (any hashref carrying a `$type`), so embeds produced by `videoEmbed` can be posted directly.

### Changed
- Video upload now targets the video service host (`https://video.bsky.app`) and authenticates with a short-lived service token minted via `com.atproto.server.getServiceAuth`, sent as a plain `Bearer` token. `uploadVideoJob` previously posted to the PDS with the session's DPoP token, which returns `501 Method Not Implemented`. The `did`/`name` query parameters and return value (`jobStatus`, including `409 AlreadyExists`) are unchanged.

### Fixed
- `getVideoUploadLimits` and `getVideoJobStatus` now use the video-service host and service token as well, so they work with OAuth (DPoP) sessions.

## [1.6.0] - 2026-10-10

### Added
- New feed methods: `getListFeed` (`app.bsky.feed.getListFeed`), `sendInteractions` (`app.bsky.feed.sendInteractions`).
- New social graph methods: `getListBlocks`, `getListMutes`, `getListsWithMembership`, `getStarterPacksWithMembership`, `getSuggestedFollowsByActor`, `searchStarterPacks`.
- New notification methods: `getNotificationPreferences`, `putNotificationPreferences`, `putNotificationPreferencesV2`, `listActivitySubscriptions`, `putActivitySubscription`, `registerPush`, `unregisterPush`.
- New video service methods: `getVideoUploadLimits`, `getVideoJobStatus`, `uploadVideoJob` (`app.bsky.video.uploadVideo`).
- New drafts support: `getDrafts`, `createDraft`, `updateDraft`, `deleteDraft`.
- New contact-import support: `getContactMatches`, `getContactSyncStatus`, `importContacts`, `dismissContactMatch`, `removeContactData`, `startPhoneVerification`, `verifyPhone`.
- New age assurance support: `beginAgeAssurance`, `getAgeAssuranceConfig`, `getAgeAssuranceState`.
- New label lookup: `queryLabels` (`com.atproto.label.queryLabels`).
- Documented new `posts_with_video` filter value for `getAuthorFeed`.
- `follower` rule for `createPost` thread gates (`reply_gate`), per the official thread gates guide.
- `uploadVideoJob` accepts `did`, `name`, and `host` options for the video-service upload flow.
- Session persistence helpers: `save_session`, `load_session`, and `ensure_auth` (resumes a saved session when present, otherwise runs OAuth and persists the result).
- Mock-based unit tests covering every network-mockable method (`t/004_full.t`), plus session-persistence tests (`t/005_session.t`).
- Local `eg/` demos: `feed.pl`, `graph.pl`, `chat.pl`, `media.pl`, and `drafts.pl`; each authenticates through `ensure_auth`.

### Changed
- Test suite migrated from `Test::More` to `Test2::V0`.

### Fixed
- `uploadVideo` now throws the server error on upload failure instead of returning early.
- `uploadFile`, `uploadImages`, `uploadVideo`, and `uploadVideoJob` now treat file paths as files on Windows (the filename guard was rejecting real paths containing `:` or `\`) and propagate the actual upload error instead of a generic "upload failed".
- Documentation. Like, in general.

## [1.01] - 2026-03-12

### Added
- Adding support for `app.bsky.feed.postgate`.

### Changed
- Minor doc fixes.

## [1.00] - 2026-03-09
### Added
- Added `oauth_helper` method to provide a streamlined, interactive OAuth flow with an optional built-in redirect listener.
- Implemented functional Chat (Direct Messages) support via PDS proxying (`atproto-proxy`).
- Added `threadgate` support to `createPost` via the `reply_gate` parameter, allowing users to control who can reply to their posts.
- Added `getKnownFollowers` method for advanced social discovery (mutual followers).
- Added `report` method for official content reporting via Ozone (`com.atproto.moderation.createReport`).
- Added robust service-aware routing in `_at_for` to handle repo, feed, and chat lexicons transparently.
- Updated examples: `eg/bsky_auth.pl` and `eg/bsky_chat.pl` are now fully operational.
- Implemented missing methods: `repost`, `deleteRepost`, `uploadBlob`, `deleteBlock`, `follow`, `deleteFollow`, `getFollows`, `getFollowers`, `getRepostedBy`.

### Changed
- Refactored all repository-related sugary methods (`repost`, `like`, `block`, `follow`, `createPost`, `upsertProfile`, etc.) to use new high-level `At.pm` helpers, resulting in a cleaner and more maintainable codebase.
- Standardized internal implementation to use accessor methods (e.g., `$self->at`) instead of direct field access, improving testability and robustness.

### Fixed
- Fixed `oauth_helper` to use standard `Mojolicious` objects instead of `Mojolicious::Lite`, resolving "Modification of a read-only value" errors.
- Corrected OAuth scopes to use `transition:generic` and `transition:chat.bsky` for reliable chat authorization.
- Fixed sender handle display in `bsky_chat.pl` by mapping member DIDs to handles.
- Support for Bookmarks: `getBookmarks`, `createBookmark`, `deleteBookmark`.
- Feed Generator methods: `getFeed`, `getFeedSkeleton`, `getFeedGenerator`, `getFeedGenerators`, `getActorFeeds`, `getSuggestedFeeds`, `getPopularFeedGenerators`, `getTrends`, `describeFeedGenerator`.
- Social Graph features: `getRelationships`, `getMutes`, `muteThread`, `unmuteThread`, `getLists`, `getList`, `getKnownFollowers`, `getSuggestedFollowsByActor`.
- Actor features: `upsertProfile`, `getProfiles`, `getSuggestions`, `getSuggestionsSkeleton`, `searchActors`, `searchActorsTypeahead`, `mute`, `unmute`, `muteModList`, `unmuteModList`, `blockModList`, `unblockModList`, `getPreferences`, `putPreferences`.
- Notification features: `listNotifications`, `countUnreadNotifications`, `updateSeenNotifications`, `putNotificationPreferences`.
- Identity features: `resolveHandle`, `updateHandle`.
- Starter Pack support: `getStarterPack`, `getStarterPacks`, `getActorStarterPacks`.
- Drafts support: `getDrafts`, `createDraft`, `updateDraft`, `deleteDraft`.
- Chat (Direct Messages) support: `listConvos`, `getConvo`, `getConvoForMembers`, `getMessages`, `sendMessage`, `sendMessageToHandle`, `updateRead`, `muteConvo`, `unmuteConvo`.
- Video features: `getVideoUploadLimits`, `getVideoJobStatus`.
- Contact features: `importContacts`, `getContactMatches`.
- Miscellaneous methods: `describeServer`, `listRecords`, `getLabelerServices`.
- OAuth and Firehose wrapper methods.
- New examples (Bluesky auth, chat, firehose, etc.)
- Refactored `uploadFile` to use `HTTP::Tiny` directly, bypassing bugs in the underlying `At` module's UserAgent.

## [0.20] - 2024-12-31

### Added
- List, create, and delete records to block actors/accounts by name.
- Allow posts to be liked by their AT-URI.
- Allow likes to be deleted.
- Wrapping Bluesky's new `*.getTrendingTopics` lexicon.

## [0.19] - 2024-12-03

### Changed
- Split from At.pm.

[Unreleased]: https://github.com/sanko/Bluesky.pm/compare/1.6.0...HEAD
[1.6.0]: https://github.com/sanko/Bluesky.pm/compare/1.01...1.6.0
[1.01]: https://github.com/sanko/Bluesky.pm/compare/1.00...1.01
[1.00]: https://github.com/sanko/Bluesky.pm/compare/0.20...1.00
[0.20]: https://github.com/sanko/Bluesky.pm/compare/0.19...0.20
[0.19]: https://github.com/sanko/Bluesky.pm/releases/tag/0.19
