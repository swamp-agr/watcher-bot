-- * common

let Prelude = ../common/map.dhall
let Entry = Prelude.Entry
let Map = Prelude.Map
let map = Prelude.map

let UTCTime = { date : Date, time : Time, timeZone : TimeZone }

-- * previous

-- ** blocklist

let PrevBanState
  = { bannedMessages : List Text
    , bannedChats : List Integer
    }

let PrevBlocklist = Map Integer PrevBanState

-- ** groups

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

let PrevChats = Map Integer PrevChatState

-- ** events

let PrevSelfDestructMessage =
      { selfDestructMessageChatId : Integer
      , selfDestructMessageId : Integer
      , selfDestructMessageTime :
          { date : Date, time : Time, timeZone : TimeZone }
      }

let PrevUserChatMemberCheck =
      { userChatMemberCheckChatId : Integer
      , userChatMemberCheckUserId : Integer
      , userChatMemberCheckTime :
          { date : Date, time : Time, timeZone : TimeZone }
      }

let PrevEvent =
  < SelfDestructMessageEvent : PrevSelfDestructMessage
  | UserChatMemberCheckEvent : PrevUserChatMemberCheck
  >

-- * new

-- ** groups

let UserInfo =
      { userInfoId : Integer
      , userInfoFirstName : Text
      , userInfoLastName : Optional Text
      , userInfoUsername : Optional Text
      , userInfoLink : Text
      }


let MessageInfo =
      { messageInfoId : Integer
      , messageInfoThreadId : Optional Integer
      , messageInfoText : Optional Text
      , messageInfoFrom : Optional UserInfo
      , messageInfoChat : PrevChatInfo
      }

let AdminCall =
      { _1 : UserInfo
      , _2 : MessageInfo
      }

let Poll =
      { pollMessageId : Integer
      , pollSpamMessageId : Integer
      , pollVoters : List Integer
      , pollSpamer : UserInfo
      }

let ChatState =
      { chatSettings : PrevChatSettings
      , chatAdmins : List Integer
      , chatAdminsCheckedAt : Optional Date
      , chatSetup : PrevChatSetupState
      , quarantine : Map Integer PrevQuarantine
      , activePolls : Map Integer Poll
      , adminCalls : Map Integer AdminCall
      , allowlist : List Integer
      , botIsAdmin : Bool
      }
      
let Chats = Map Integer ChatState

-- ** Events

let UserChatMemberCheck =
      { userChatMemberCheckChatId : Integer
      , userChatMemberCheckUserInfo : UserInfo
      , userChatMemberCheckTime :
          { date : Date, time : Time, timeZone : TimeZone }
      }

let Event =
  < SelfDestructMessageEvent : PrevSelfDestructMessage
  | UserChatMemberCheckEvent : UserChatMemberCheck
  >

-- * migrate :)

let migrateBlocklist
  = λ(old: PrevBlocklist)
  → { spamerBans = old
    , spamerUsernames = [] : List { mapKey : Text, mapValue : Integer }
    }

let migrateUserInfo
  = λ(old: PrevUserInfo)
  → old ⫽ { userInfoUsername = None Text }

let migrateOptionalUserInfo
  = λ(old: Optional PrevUserInfo)
  → merge {
      None = None UserInfo,
      Some = λ(v: PrevUserInfo) → Some (migrateUserInfo v)
    } old

let migrateActivePolls
  = λ(old: Entry Integer PrevPoll)
  → { mapKey = old.mapKey
    , mapValue = old.mapValue ⫽ { pollSpamer = migrateUserInfo old.mapValue.pollSpamer }
    }

let migrateAdminCalls
  = λ(old: Entry Integer PrevAdminCall)
  → { mapKey = old.mapKey
    , mapValue =
        { _1 = migrateUserInfo old.mapValue._1
        , _2 = old.mapValue._2
          ⫽ { messageInfoFrom = migrateOptionalUserInfo old.mapValue._2.messageInfoFrom }
        }
    }

let migrateChatState
  = λ(old: PrevChatState)
  → old ⫽ { adminCalls = map
              (Entry Integer PrevAdminCall) (Entry Integer AdminCall)
              migrateAdminCalls old.adminCalls
          , activePolls = map
              (Entry Integer PrevPoll) (Entry Integer Poll)
              migrateActivePolls old.activePolls
          }
let migrateGroups
  = λ(old: Entry Integer PrevChatState)
  → { mapKey = old.mapKey
    , mapValue = migrateChatState old.mapValue
    }

let makeUserInfo
  = λ(old: Integer)
  → { userInfoId = old
    , userInfoFirstName = ""
    , userInfoLastName = None Text
    , userInfoUsername = None Text
    , userInfoLink = "<a href='tg://chat?id=${Integer/show old}'>N/A</a>"
    }

let migrateEvents
  = λ(old: PrevEvent)
  → merge { 
      SelfDestructMessageEvent
        = λ(old : PrevSelfDestructMessage) → Event.SelfDestructMessageEvent old,
      UserChatMemberCheckEvent
        = λ(old : PrevUserChatMemberCheck) → Event.UserChatMemberCheckEvent
            { userChatMemberCheckChatId = old.userChatMemberCheckChatId
            , userChatMemberCheckUserInfo = makeUserInfo old.userChatMemberCheckUserId
            , userChatMemberCheckTime = old.userChatMemberCheckTime
            }
    } old

in { migrateBlocklist
   , migrateGroups =
       map (Entry Integer PrevChatState) (Entry Integer ChatState) migrateGroups
   , migrateEvents = map PrevEvent Event migrateEvents
   }
