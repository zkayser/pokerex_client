module Types.Room.ModalState exposing (ModalState(..), BottomModalType(..))

import Data.WinningHand as WinningHand exposing (WinningHand)

type ModalState
  = Closed
  | JoinModalOpen
  | BankModalOpen
  | BottomModalOpen BottomModalType
  | RaiseModalOpen
  | WinningHandModal WinningHand

type BottomModalType
  = Actions
  | Account
  | Chat
  | MobileMenu