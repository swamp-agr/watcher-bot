let Map = https://prelude.dhall-lang.org/Map/Type
let Map/empty = https://prelude.dhall-lang.org/Map/empty
let Communication =
      < LongPolling
      | Webhook
        : { webhookCertPath : Text
          , webhookKeyPath : Text
          , webhookPort : Natural
          , webhookHost : Text
          }
      >
let OwnerGroup =
      { ownerGroupId : Integer
      , ownerGroupSpamThreadId : Integer
      , ownerGroupTuningThreadId : Integer
      , ownerGroupFeedbackThreadId : Integer
      , ownerGroupStatsThreadId : Integer
      , ownerGroupBackupThreadId : Integer
      }
let SpamCommandAction = < SCPoll | SCAdminsCall >
let ScoreSettings =
      { scoreUserHasNoUsername : Natural
      , scoreUserNameContainsEmoji : Natural
      , scoreUserHasPremium : Natural
      , scoreUserAdultScore : Natural
      , scoreUserKnownSpamerNames : Map Text Natural
      , scoreMessageContainsRichMarkup : Natural
      , scoreMessageWordsScore : Map Text Natural
      , scoreAdultEmoji : Text
      , scoreMajorThreshold : Natural
      , scoreCriticalThreshold : Natural
      , scoreCopyPaste : Natural
      }
let _publicHelp =
    ''
\* Launch `/setup` command and follow the instructions if they are present\.
\* Reply with `/spam` command on a spam message\. Use `/s` as a shortcut for `/spam`\.
\* Contact bot owners with `/contact` command\.
    ''
let _adminHelp =
    ''
\* First launch `/setup` command in the group\.
  Then send `/setup` as direct message to bot and follow the instructions\.
\* Reply with `/spam` command on a spam message in a public chat
  to immediately ban spamer\. Remember, if regular user invokes the command,
  it will trigger the setting that has been set by administrators\.
\* Use `/s` as a shortcut for `/spam` command\.
\* Unban accidentally banned user with `/undo @username`
  \(replace it with real username of the user\)
  or `/undo <user_id>` \(again, replace `<user_id>` with userId\),
  e\.g\. `/undo 1` will unban Pavel Durov in the chat\.
\* Also there is a shortcut for unbanning: `/u`\.
\* Contact bot owners with `/contact` command\.
  *Tip*: you can include your feedback right after the command in that same message\.
    ''
let WorkerPeriod = < Second | Minute | Hour | Day | Week >
let WorkerMode =
  < WorkerRange : { workerRangeFrom : Time, workerRangeTo : Time }
  | WorkerAt : { workerAt : Time }
  >
in
{ botName = env:WATCHER_BOT_NAME as Text
, botToken = env:WATCHER_BOT_TOKEN as Text
, ownerGroup =
    Some { ownerGroupId = env:WATCHER_BOT_OWNER_GROUP
         , ownerGroupSpamThreadId = +2
         , ownerGroupTuningThreadId = +4
         , ownerGroupFeedbackThreadId = +3
         , ownerGroupStatsThreadId = +5
         , ownerGroupBackupThreadId = +1626
         } 
, debugEnabled = False
, defaultGroupSettings =
    { usersForConsensus = +3
    , spamCommandAction = SpamCommandAction.SCPoll
    , messagesInQuarantine = +5
    , selfDestroyEnabled = True
    }
, scores =
    { scoreUserHasNoUsername = 200
    , scoreUserNameContainsEmoji = 200
    , scoreUserHasPremium = 200
    , scoreUserAdultScore = 400 -- per emoji
    , scoreUserKnownSpamerNames = 
        [ { mapKey = "милана", mapValue = 100 }
        , { mapKey = "ксения", mapValue = 100 }
        , { mapKey = "эмма", mapValue = 100 }
        , { mapKey = "полина", mapValue = 100 }
        , { mapKey = "диана", mapValue = 100 }
        , { mapKey = "эльвира", mapValue = 100 }
        , { mapKey = "светлана", mapValue = 100 }
        ]
    , scoreMessageContainsRichMarkup = 30 -- per word
    , scoreMessageWordsScore = ./dictionary.dhall
    , scoreAdultEmoji = "🍀🔫💨💊❄️🍁❣️🍆🐇🐰🔞🥰❤️😘🌹😜🍑💞💋🍒🥵🌈💛👅👄🍌🔥💦💝"
    , scoreMajorThreshold = 500
    , scoreCriticalThreshold = 1000
    , scoreCopyPaste = 500
    } : ScoreSettings
, helpSettings =
    { publicHelp = _publicHelp
    , adminHelp = _adminHelp
    , ownerHelp =
    ''
Regular user help:

${_publicHelp}

Chat admin help:

${_adminHelp}

Extra commands:
\* `/tuning` \(as reply calculate message score of the forwarded message inside owner's group\)\.
    ''
    }
, storage =
    { groupsPath = "groups"
    , adminsPath = "admins"
    , usersPath = "users"
    , blocklistPath = "blocklist"
    , spamMessagesPath = "spam_messages"
    , eventSetPath = "triggered_events"
    , archiveDir = "backups"
    }
, analytics =
    { analyticsDir = "./bigdata"
    , chatEventsPath = "chat_events.csv"
    , userEventsPath = "user_events.csv"
    , restEventsPath = "other_events.csv"
    }
, workers =
    { cleanup =
        { workerName = "cleanup"
        , workerPeriod = WorkerPeriod.Day
        , workerPeriodUnits = 1
        , workerMode = Some (WorkerMode.WorkerAt { workerAt = 01:00:00 })
        }
    , dump =
        { workerName = "dump"
        , workerPeriod = WorkerPeriod.Hour
        , workerPeriodUnits = 2
        , workerMode = None WorkerMode
        }
    , statistics =
        { workerName = "statistics"
        , workerPeriod = WorkerPeriod.Hour
        , workerPeriodUnits = 4
        , workerMode = Some (WorkerMode.WorkerRange
            { workerRangeFrom = 07:00:00
            , workerRangeTo = 23:59:59
            })
        }
    }
, cas =
    { casEnabled = False
    , casEndpoint = "https://api.cas.chat/check?user_id="
    , casTimeoutMs = 200
    }
, communication = Communication.LongPolling
}
