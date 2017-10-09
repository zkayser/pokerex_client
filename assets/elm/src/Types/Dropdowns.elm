module Types.Dropdowns exposing (..)

type OpenDropdown
  = AllClosed
  | NavBarDropdown

type DropdownMsg
  = Toggle OpenDropdown
  | NavItemPicked String
  | Blur