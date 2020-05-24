# reflex-mirror

This is all very WIP. The interface does not pretend to be stable. As I need things, I change/write things.

So far a lot is available, but from `Reflex.Adjustable.Class.Adjustable`, only `runWithReplace` is currently 
implemented (the others are `undefined`). There is also no clean separation of implementation and interface
as in `Reflex.DOM`. And I hacked in something to make what I have of `Adjustable` work - I was intending to 
try re-reading `Reflex.DOM` to get a better understanding of how it does it [altho I think it relies too heavily
on the DOM tree structure to actually be adapatable here]. Also, I doubt Reflex.Time will work without work. 

So far: 

  * Windows whose size is user controlled
  * Windows whose size is code controlled
  * Ability to receive and respond to window messages
  * Ability to receive WM_COMMAND messages via the widget to which they relate
  * In principle, all the basic controls, tho you may need to write extra Win32 constants or something
  * Toolbar - still needs work, state updates are only availble 
  * DPI change widget, to allow you to specify sizes in nominal pixels
  * Theme font widget, to allow you to load 
  * enough extra bindings to let you draw in the title bar 
    (my app has tabs in the titlebar like firefox)
    
Still missing:

  * Accelerators - need changes to the `messageLoop`. also need to expose raw WM_COMMANDs on the window 
    which receives them
  * Non-modal dialog boxes - I presume, since these need changes to the `messageLoop`
  * Menus - need to actually get the menu 
  * Nice api to receive WM_NOTIFY messages via the widget to which they relate
  * Presumably also some convenient way of making a child widget respond to a parent message sent in
    relation to the child widget (e.g. `WM_CTLCOLORSTATIC`)
  * Anything else 
