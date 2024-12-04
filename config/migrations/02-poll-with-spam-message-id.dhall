let Prelude/map = ../common/map.dhall

let UTCTime = { date : Date, time : Time, timeZone : TimeZone }

let PrevPoll =
      { mapKey : Integer
      , mapValue :
          { pollMessageId : Integer
          , pollVoters : List Integer
          , pollSpamer :
                { userInfoId : Integer
                , userInfoFirstName : Text
                , userInfoLastName : Optional Text
                , userInfoLink : Text
                }
          }
      }

let PrevAdminCall =
      { mapKey : Integer
      , mapValue :
          { _1 :
              { userInfoId : Integer
              , userInfoFirstName : Text
              , userInfoLastName : Optional Text
              , userInfoLink : Text
              }
          , _2 :
              { messageInfoId : Integer
              , messageInfoThreadId : Optional Integer
              , messageInfoText : Optional Text
              , messageInfoFrom :
                  Optional
                    { userInfoId : Integer
                    , userInfoFirstName : Text
                    , userInfoLastName : Optional Text
                    , userInfoLink : Text
                    }
              , messageInfoChat :
                  { chatInfoId : Integer
                  , chatInfoName : Text
                  , chatInfoTitle : Optional Text
                  , chatInfoBio : Optional Text
                  , chatInfoEmojiStatusCustomEmojiId : Optional Text
                  , chatInfoBackgroundCustomEmojiId : Optional Text
                  }
              }
          }
      }

let PrevChatSettings =
      { messagesInQuarantine : Integer
      , spamCommandAction : < SCAdminsCall | SCPoll >
      , usersForConsensus : Integer
      }

let PrevSetupState =
      < SetupCompleted :
          { setupCompletedAt : UTCTime
          , setupCompletedByAdmin : Integer
          }
      | SetupInProgress :
          { setupModifiedAt : UTCTime
          , setupModifiedByAdmin : Integer
          }
      | SetupNone
      >

let PrevQuarantine =
      { mapKey : Integer
      , mapValue :
          { _1 : Optional
              { chatInfoBackgroundCustomEmojiId : Optional Text
              , chatInfoBio : Optional Text
              , chatInfoEmojiStatusCustomEmojiId : Optional Text
              , chatInfoId : Integer
              , chatInfoName : Text
              , chatInfoTitle : Optional Text
              }
          , _2 : List Bytes
          }
      }

let PrevChatState =
      { activePolls : List PrevPoll
      , adminCalls : List PrevAdminCall
      , allowlist : List Integer
      , botIsAdmin : Bool
      , chatAdmins : List Integer
      , chatAdminsCheckedAt : Optional Date
      , chatSettings : PrevChatSettings
      , chatSetup : PrevSetupState
      , quarantine : List PrevQuarantine
      }

let PrevChats = { mapKey : Integer, mapValue : PrevChatState }

-- New types

let Poll =
      { mapKey : Integer
      , mapValue :
          { pollMessageId : Integer
          , pollSpamMessageId : Integer
          , pollVoters : List Integer
          , pollSpamer :
                { userInfoId : Integer
                , userInfoFirstName : Text
                , userInfoLastName : Optional Text
                , userInfoLink : Text
                }
          }
      }

let ChatState =
      { activePolls : List Poll
      , adminCalls : List PrevAdminCall
      , allowlist : List Integer
      , botIsAdmin : Bool
      , chatAdmins : List Integer
      , chatAdminsCheckedAt : Optional Date
      , chatSettings : PrevChatSettings
      , chatSetup : PrevSetupState
      , quarantine : List PrevQuarantine
      }

let Chats = { mapKey : Integer, mapValue : ChatState }

-- migrate

let migratePoll
  = λ(old : PrevPoll)
  → old ⫽ { mapValue = old.mapValue ⫽ { pollSpamMessageId = -1 } } -- dummy value is enough

let migrateChatState
  = λ(old : PrevChatState)
  → old ⫽
      { activePolls = Prelude/map PrevPoll Poll migratePoll old.activePolls
      }
let migrateGroups
  = λ(old : PrevChats)
  → { mapKey = old.mapKey
    , mapValue = migrateChatState old.mapValue
    }

in { migrateGroups = Prelude/map PrevChats Chats migrateGroups }

-- Example:
--
-- let groups = ./cache/2024-12-03/groups/1733255948.dhall
-- in (./config/migrations/02-poll-with-spam-message-id.dhall).migrateGroups groups
--
-- OK!
