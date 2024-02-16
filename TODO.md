* `Malef`
 * `Debug_IO`
  - [ ] Implement the Widget
 * Drawing
  * `Surfaces`
   - [ ] Copy subsurface onto another surface
   - [ ] Tests
  * `Groups`
   - [ ] Add tampering checks to groups
   - [ ] Add palettes to groups
   - [ ] Add contracts to all functions
   - [ ] Add controlled functions so that group contents are freed on destruction
   - [ ] Finish tests
  * `Events`
   - [ ] Add special Keys
  * `Glyphs`
   - [ ] Add a way to check unicode compatibility
  * `Window`
   - [ ] Move semantics for Groups
   - [ ] Get a reference from the internal surface
   - [ ] Keep track of current position
   - [ ] Add events:
    - [ ] Resize
    - [ ] Keyboard
    - [ ] Mouse
    - [ ] Cancel (Ctrl-C)
    - [ ] Close (Ctrl-D)
    - [ ] Kill (SEGKILL)
   - [ ] Tests
  * `Palettes`
   - [ ] Add more palette operations
   - [ ] Get internal palette
   - [ ] Tests
 * Subsystems
  * `CMD`
   - [ ] Alternative characters
   - [ ] Use ANSI if possible
 * Widgets
  * `Application`
   - [ ] Handle Dialog positions
   - [ ] Minimise redrawings
   - [ ] Events
   - [ ] Tests
  * `Widgets`
   - [ ] Finish the interface
   - [ ] Add reference to parent in functions so that they can be referenced.
  * `Styles`
   - [ ] Find a way to represent a Style internally
   - [ ] Allow declaring styles using Ada Aggregates
   - [ ] Allow loading styles from CSS-like language
  * `Dialogs`
   - [ ] Make it generic for any Area size
   - [ ] Make it use the given style
  * `Labels`
   - [ ] Left-Right-Top-Bottom
   - [ ] Right-Left-Bottom-Top
   - [ ] Top-Bottom-Right-Left
   - [ ] Minimise redrawing
   - [ ] Plain Text
   - [ ] Markdown
   - [ ] Code
  * `Buttons`
  * `Radio_Buttons`
  * `Check_Buttons`
  * `Field` (Text, Numeric, ..., Constrained, Unconstrained) [Static_Predicate]
  * `Scrollers`
