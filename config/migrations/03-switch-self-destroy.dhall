-- * common

let Prelude/map = ../common/map.dhall

let UTCTime = { date : Date, time : Time, timeZone : TimeZone }

let IntTextMap = { mapKey : Integer, mapValue : Optional Text }

-- * previous

-- ** groups

let PrevPoll =
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

let PrevChatState =
      { activePolls : List PrevPoll
      , adminCalls : List PrevAdminCall
      , allowlist : List Integer
      , botIsAdmin : Bool
      , chatAdmins : List Integer
      , chatAdminsCheckedAt : Optional Date
      , chatSettings : PrevChatSettings
      , chatSetup : PrevChatSetupState
      , quarantine : List PrevQuarantine
      }

let PrevChats = { mapKey : Integer, mapValue : PrevChatState }

-- ** Users

let PrevMenuId =
    < MenuRoot
    | ConsensusRoot
    | SpamCmdRoot
    | QuarantineRoot
    | Done
    | Multi : Integer
    | Consensus : Integer
    | SpamCmd : < SCPoll | SCAdminsCall >
    | Quarantine : Integer
    | BotIsAdmin
    >


let PrevMenuSettings =
      < MultipleGroupsRoot :
          { multipleGroupsMenu : List IntTextMap }
      | MultipleGroupsSelected :
          { multipleGroupsSelectedChat : Integer
          , multipleGroupsSelectedSubMenu : PrevMenuId
          , multipleGroupsSelectedMenu : List IntTextMap
          }
      | SingleRoot :
          { singleGroupChat : Integer
          , singleGroupSubMenu : PrevMenuId
          }
      >

let PrevSetupState =
    < UserSetupNone
    | UserSetupInProgress :
        { userSetupMenu : PrevMenuSettings
        , userSetupMessageId : Optional Integer
        }
    >

let PrevUserSetupState =
      { userSetupState : PrevSetupState
      , userContactState : < UserContactNone | UserContactInProgress >
      , userCurrentState : Optional < UserCurrentSetup | UserCurrentContact >
      }

let PrevUsers = { mapKey : Integer, mapValue : PrevUserSetupState }

-- * New types

-- ** Groups

let ChatSettings =
      { messagesInQuarantine : Integer
      , spamCommandAction : < SCAdminsCall | SCPoll >
      , usersForConsensus : Integer
      , selfDestroyEnabled : Bool
      }

let ChatState =
      { activePolls : List PrevPoll
      , adminCalls : List PrevAdminCall
      , allowlist : List Integer
      , botIsAdmin : Bool
      , chatAdmins : List Integer
      , chatAdminsCheckedAt : Optional Date
      , chatSettings : ChatSettings
      , chatSetup : PrevChatSetupState
      , quarantine : List PrevQuarantine
      }

let Chats = { mapKey : Integer, mapValue : ChatState }

-- ** users

let MenuId =
    < MenuRoot
    | ConsensusRoot
    | SpamCmdRoot
    | QuarantineRoot
    | SelfDestroyRoot
    | Done
    | Multi : Integer
    | Consensus : Integer
    | SpamCmd : < SCPoll | SCAdminsCall >
    | Quarantine : Integer
    | BotIsAdmin
    | SelfDestroy : Bool
    >


let MenuSettings =
      < MultipleGroupsRoot :
          { multipleGroupsMenu : List IntTextMap }
      | MultipleGroupsSelected :
          { multipleGroupsSelectedChat : Integer
          , multipleGroupsSelectedSubMenu : MenuId
          , multipleGroupsSelectedMenu : List IntTextMap
          }
      | SingleRoot :
          { singleGroupChat : Integer
          , singleGroupSubMenu : MenuId
          }
      >

let SetupState =
    < UserSetupNone
    | UserSetupInProgress :
        { userSetupMenu : MenuSettings
        , userSetupMessageId : Optional Integer
        }
    >

let UserSetupState =
      { userSetupState : SetupState
      , userContactState : < UserContactNone | UserContactInProgress >
      , userCurrentState : Optional < UserCurrentSetup | UserCurrentContact >
      }

let Users = { mapKey : Integer, mapValue : UserSetupState }

-- * migrate

-- ** groups

let migrateChatState
  = λ(old : PrevChatState)
  → old ⫽
      { chatSettings = old.chatSettings
        ⫽ { selfDestroyEnabled = True }
      }

let migrateGroups
  = λ(old : PrevChats)
  → { mapKey = old.mapKey
    , mapValue = migrateChatState old.mapValue
    }

-- ** users

let migrateMenuId
  = λ(old: PrevMenuId)
  → merge {
    MenuRoot = MenuId.MenuRoot,
    ConsensusRoot = MenuId.ConsensusRoot,
    SpamCmdRoot = MenuId.SpamCmdRoot,
    QuarantineRoot = MenuId.QuarantineRoot,
    Done = MenuId.Done,
    Multi = λ(v: Integer) → MenuId.Multi v,
    Consensus = λ(v: Integer) → MenuId.Consensus v,
    SpamCmd = λ(v: < SCPoll | SCAdminsCall >) → MenuId.SpamCmd v,
    Quarantine = λ(v: Integer) → MenuId.Quarantine v,
    BotIsAdmin = MenuId.BotIsAdmin
  } old

let migrateMenuSettings
  = λ(oldMenuSettings : PrevMenuSettings)
  → merge {
    MultipleGroupsRoot
      = λ(v: { multipleGroupsMenu : List IntTextMap})
      → MenuSettings.MultipleGroupsRoot { multipleGroupsMenu = v.multipleGroupsMenu },
    MultipleGroupsSelected
      = λ(v: { multipleGroupsSelectedChat : Integer
             , multipleGroupsSelectedSubMenu : PrevMenuId
             , multipleGroupsSelectedMenu : List IntTextMap
             })
      → MenuSettings.MultipleGroupsSelected
          { multipleGroupsSelectedSubMenu = migrateMenuId v.multipleGroupsSelectedSubMenu
          , multipleGroupsSelectedChat = v.multipleGroupsSelectedChat
          , multipleGroupsSelectedMenu = v.multipleGroupsSelectedMenu
          },
    SingleRoot
      = λ(v: { singleGroupChat : Integer
             , singleGroupSubMenu : PrevMenuId
             })
      → MenuSettings.SingleRoot
          { singleGroupSubMenu = migrateMenuId v.singleGroupSubMenu
          , singleGroupChat = v.singleGroupChat
          }
  } oldMenuSettings

let migrateSetup
  = λ(oldSetup : PrevSetupState)
  → merge {
  UserSetupNone = SetupState.UserSetupNone,
  UserSetupInProgress
    = λ(v: { userSetupMenu : PrevMenuSettings
           , userSetupMessageId : Optional Integer
           })
    → SetupState.UserSetupInProgress
        { userSetupMenu = migrateMenuSettings v.userSetupMenu
        , userSetupMessageId = v.userSetupMessageId
        }
  } oldSetup

let migrateUserSetup
  = λ(old : PrevUserSetupState)
  → old ⫽ { userSetupState = migrateSetup old.userSetupState }

let migrateUsers
  = λ(oldUser : PrevUsers)
  → { mapKey = oldUser.mapKey
    , mapValue = migrateUserSetup oldUser.mapValue
    }

in { migrateGroups = Prelude/map PrevChats Chats migrateGroups
   , migrateUsers = Prelude/map PrevUsers Users migrateUsers
   }

-- Example:
--
-- let groups = ./cache/2024-12-04/groups/1733339230.dhall
-- in (./config/migrations/03-switch-self-destroy.dhall).migrateGroups groups
--
--
-- OK!
