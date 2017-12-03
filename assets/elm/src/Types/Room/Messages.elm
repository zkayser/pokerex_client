module Types.Room.Messages exposing ( RoomMsg(..), RoomExternalMsg(..))

import Data.Player as Player exposing (Player)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Time exposing (Time)

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

type RoomExternalMsg
  = NoOp