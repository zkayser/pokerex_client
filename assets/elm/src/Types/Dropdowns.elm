module Types.Dropdowns exposing (..)

type OpenDropdown
  = AllClosed
  | NavBarDropdown

type DropdownMsg
  = Toggle OpenDropdown
  | NavItemPicked String
  | Blur

type DropdownItem
  = None
  | AnyItem String