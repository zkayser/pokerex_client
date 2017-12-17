module Types.Dropdowns exposing (..)

type OpenDropdown
  = AllClosed
  | NavBarDropdown

type DropdownMsg
  = Toggle OpenDropdown
  | NavItemPicked DropdownNavbarLink
  | Blur

type DropdownItem
  = None
  | AnyItem String

type DropdownNavbarLink
  = Logout
  | Login
  | Register
  | Room -- TODO: Change this back to `Rooms` when the route is ready to go.
  | Profile