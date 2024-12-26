-- * common

let Prelude = ../common/map.dhall
let Entry = Prelude.Entry
let Map = Prelude.Map
let map = Prelude.map

let UTCTime = { date : Date, time : Time, timeZone : TimeZone }

-- * previous

let PrevChatSettings =
      { messagesInQuarantine : Integer
      , spamCommandAction : < SCAdminsCall | SCPoll >
      , usersForConsensus : Integer
      , selfDestroyEnabled : Bool
      }

let PrevChatSetupState =
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

let PrevUserInfo =
      { userInfoId : Integer
      , userInfoFirstName : Text
      , userInfoLastName : Optional Text
      , userInfoUsername : Optional Text
      , userInfoLink : Text
      }


let PrevChatInfo =
      { chatInfoId : Integer
      , chatInfoName : Text
      , chatInfoTitle : Optional Text
      , chatInfoBio : Optional Text
      , chatInfoEmojiStatusCustomEmojiId : Optional Text
      , chatInfoBackgroundCustomEmojiId : Optional Text
      }

let PrevMessageInfo =
      { messageInfoId : Integer
      , messageInfoThreadId : Optional Integer
      , messageInfoText : Optional Text
      , messageInfoFrom : Optional PrevUserInfo
      , messageInfoChat : PrevChatInfo
      }

let PrevQuarantine =
      { _1 : Optional PrevChatInfo
      , _2 : List Bytes
      }

let PrevAdminCall =
      { _1 : PrevUserInfo
      , _2 : PrevMessageInfo
      }

let PrevPoll =
      { pollMessageId : Integer
      , pollSpamMessageId : Integer
      , pollVoters : List Integer
      , pollSpamer : PrevUserInfo
      }


let PrevChatState =
      { chatSettings : PrevChatSettings
      , chatAdmins : List Integer
      , chatAdminsCheckedAt : Optional Date
      , chatSetup : PrevChatSetupState
      , quarantine : Map Integer PrevQuarantine
      , activePolls : Map Integer PrevPoll
      , adminCalls : Map Integer PrevAdminCall
      , allowlist : List Integer
      , botIsAdmin : Bool
      }

-- * new

let Quarantine =
      { quarantineUserChatInfo : Optional PrevChatInfo
      , quarantineMessageHash : List Bytes
      , quarantineMessageId : List Integer
      }

let ChatState =
      { chatSettings : PrevChatSettings
      , chatAdmins : List Integer
      , chatAdminsCheckedAt : Optional Date
      , chatSetup : PrevChatSetupState
      , quarantine : Map Integer Quarantine
      , activePolls : Map Integer PrevPoll
      , adminCalls : Map Integer PrevAdminCall
      , allowlist : List Integer
      , botIsAdmin : Bool
      }

-- * migrate

let migrateQuarantine
  = λ(old: Entry Integer PrevQuarantine)
  → { mapKey = old.mapKey
    , mapValue =
        { quarantineUserChatInfo = old.mapValue._1
        , quarantineMessageHash = old.mapValue._2
        , quarantineMessageId = [] : List Integer
        }
    }

let migrateChatState
  = λ(old: PrevChatState)
  → old ⫽ { quarantine = map
              (Entry Integer PrevQuarantine) (Entry Integer Quarantine)
              migrateQuarantine old.quarantine
          }

let migrateGroups
  = λ(old: Entry Integer PrevChatState)
  → { mapKey = old.mapKey
    , mapValue = migrateChatState old.mapValue
    }

in { migrateGroups =
       map (Entry Integer PrevChatState) (Entry Integer ChatState) migrateGroups
   }
