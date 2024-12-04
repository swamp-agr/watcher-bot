let Prelude/map = ../common/map.dhall

let IntTextMap = { mapKey : Integer, mapValue : Text }

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

-- New one

let IntSomeTextMap = { mapKey : Integer, mapValue : Optional Text }

let MenuSettings =
      < MultipleGroupsRoot :
          { multipleGroupsMenu : List IntSomeTextMap }
      | MultipleGroupsSelected :
          { multipleGroupsSelectedChat : Integer
          , multipleGroupsSelectedSubMenu : PrevMenuId
          , multipleGroupsSelectedMenu : List IntSomeTextMap
          }
      | SingleRoot :
          { singleGroupChat : Integer
          , singleGroupSubMenu : PrevMenuId
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

-- migrate :)

let addSome
  = λ(old: IntTextMap)
  → { mapKey = old.mapKey
    , mapValue = Some old.mapValue
    }
let migrateMenuSettings
  = λ(oldMenuSettings : PrevMenuSettings)
  → merge {
    MultipleGroupsRoot
      = λ(v: { multipleGroupsMenu : List IntTextMap})
      → MenuSettings.MultipleGroupsRoot { multipleGroupsMenu = Prelude/map IntTextMap IntSomeTextMap addSome v.multipleGroupsMenu },
    MultipleGroupsSelected
      = λ(v: { multipleGroupsSelectedChat : Integer
             , multipleGroupsSelectedSubMenu : PrevMenuId
             , multipleGroupsSelectedMenu : List IntTextMap
             })
      → MenuSettings.MultipleGroupsSelected { multipleGroupsSelectedChat = v.multipleGroupsSelectedChat
        , multipleGroupsSelectedSubMenu = v.multipleGroupsSelectedSubMenu
        , multipleGroupsSelectedMenu = Prelude/map IntTextMap IntSomeTextMap addSome v.multipleGroupsSelectedMenu
        },
    SingleRoot
      = λ(v: { singleGroupChat : Integer
             , singleGroupSubMenu : PrevMenuId
             })
      → MenuSettings.SingleRoot v
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
  → old // { userSetupState = migrateSetup old.userSetupState }

let migrateUsers
  = λ(oldUser : PrevUsers)
  → { mapKey = oldUser.mapKey
    , mapValue = migrateUserSetup oldUser.mapValue
    }

in { migrateUsers = Prelude/map PrevUsers Users migrateUsers }

-- Example:
--
-- let users = ./cache/2024-12-02/users/1733163529.dhall
-- in (./config/migrations/01-optional-chat-titles.dhall).migrateUsers users
--
-- OK!
