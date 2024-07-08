---
title: Haskell on Web
author: Mehran
---

Haskell On Web
---

<!-- column_layout: [1, 2, 1] -->

<!-- column: 1 -->
<!-- new_lines: 10 -->
# Phase 1
<!-- pause -->
## Backend
* REST
* Postgresql
* Model Designing
* Authentication (jwt)
* Memory Caching
* Session management
<!-- pause -->
## Front
* Login page
* Game form
* Finding avatar https://iconscout.com/all-assets/mafia
* Role description
* Role distribution
* Game status

# Phase 2
* Multi user access to game using websocket

<!-- end_slide -->
What we manage to do
---
<!-- column_layout: [1, 2, 1] -->

<!-- column: 1 -->
<!-- new_lines: 10 -->
## Set up a dev workflow (used Nix)
<!-- pause -->
## Project Management using Kanban board
<!-- pause -->
## Setup Handler and define routes
<!-- pause -->
## Set up a simple Schema and run on Sqlite
<!-- pause -->
## Connect React to our web
<!-- pause -->
## Using the tempalte to create some pages

<!-- end_slide -->
Setup Handler
---
```haskell

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")
```
<!-- pause -->
Setup rest route using Model
---
```haskell
postCommentR :: Handler Value
postCommentR = do
    comment <- (requireCheckJsonBody :: Handler Comment)

    maybeCurrentUserId <- maybeAuthId
    let comment' = comment { commentUserId = maybeCurrentUserId }

    insertedComment <- runDB $ insertEntity comment'
    returnJson insertedComment

getCommentR :: Handler Value
getCommentR = do
  -- Fetch comments from the database
    comments <- runDB $ selectList [] [Desc CommentId]
  -- Convert comments to JSON
    let commentsJson = Aeson.toJSON comments
  -- Return comments as JSON response
    returnJson commentsJson
```
