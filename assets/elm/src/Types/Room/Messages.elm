module Types.Room.Messages exposing ( RoomMsg(..))

import Data.Player as Player exposing (Player)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Time exposing (Time)

--type RoomMsg 
--  = Btn ButtonAction
--  | JoinMsg JoinAction
--  | Raise RaiseAction
--  | Action ActionMessage
--  | Bank BankAction
--  | Game GameMessage
--  | Chat ChatMessage
--  | Socket SocketMessage
--  | Modal ModalMessage

type RoomMsg
  = JoinRoom Player -- START BUTTON ACTION MESSAGES
  | LeaveRoom Player
  | ActionPressed
  | BankPressed
  | AccountPressed
  | ChatPressed
  | OpenRaisePressed
  | MobileToolbarPressed -- END BUTTON ACTION MESSAGES
  | Join -- START JOIN ACTION MESSAGES
  | JoinedChannel
  | JoinFailed Value
  | SetJoinValue String
  | ConnectedToPlayerChannel
  | ChipInfo Encode.Value -- END JOIN ACTION MESSAGES
  | IncreaseRaise Int -- START RAISE ACTION MESSAGES
  | DecreaseRaise Int
  | SetRaise String -- END RAISE ACTION MESSAGES
  | ActionMsg String Encode.Value -- ACTION MSG
  | SetBankInfo Value -- BANK ACTION
  | SetAddAmount String -- BANK ACTION
  | Update Value -- START MESSAGES FROM GAME SERVER
  | GameStarted Value
  | WinnerMessage Value
  | Clear Value
  | PresentWinningHand Value -- END MESSAGES FROM GAME SERVER
  | NewChatMsg Value -- START CHAT MESSAGES
  | SetChatMsg String
  | CloseModal
  | SubmitChat -- END CHAT MESSAGES
  | SocketOpened -- START SOCKET MESSAGES
  | SocketClosed
  | SocketClosedAbnormally
  | Rejoined Value -- END SOCKET MESSAGES
  | Blur -- START MODAL-RELATED MESSAGES
  | ClearErrorMessage Time
  | ClearRoomMessage Time
  | ClearWinningHandModal Time
  | CloseRaiseModal
  | CloseWinningHandModal -- END MODAL-RELATED MESSAGES


--type ButtonAction
--  = JoinRoom Player
--  | LeaveRoom Player
--  | ActionPressed
--  | BankPressed
--  | AccountPressed
--  | ChatPressed
--  | OpenRaisePressed
--  | MobileToolbarPressed

--type JoinAction
--  = Join
--  | JoinedChannel
--  | JoinFailed Value
--  | SetJoinValue String
--  | ConnectedToPlayerChannel
--  | ChipInfo Encode.Value

--type RaiseAction
--  = IncreaseRaise Int
--  | DecreaseRaise Int
--  | SetRaise String

--type ActionMessage
--  = ActionMsg String Encode.Value

--type BankAction
--  = SetBankInfo Value
--  | SetAddAmount String

--type GameMessage
--  = Update Value
--  | GameStarted Value
--  | WinnerMessage Value
--  | Clear Value
--  | PresentWinningHand Value

--type ChatMessage
--  = NewChatMsg Value
--  | SetChatMsg String
--  | CloseModal
--  | SubmitChat

--type SocketMessage
--  = SocketOpened
--  | SocketClosed
--  | SocketClosedAbnormally
--  | Rejoined Value

--type ModalMessage
--  = Blur
--  | ClearErrorMessage Time
--  | ClearRoomMessage Time
--  | ClearWinningHandModal Time
--  | CloseRaiseModal
--  | CloseWinningHandModal