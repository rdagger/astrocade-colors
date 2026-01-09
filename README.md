# Color Viewer, Color Matcher, and Color Table for Bally Astrocade

## Overview

This repo contains three utilities for the Bally Astrocade that help developers preview, compare, and select colors on real hardware. These tools make it easier to design palettes for games and demos while seeing exactly how colors appear on an actual console rather than an emulator.

![viewer](https://github.com/user-attachments/assets/f879bd83-68b0-44c4-871c-2957f7944436)
![matcher](https://github.com/user-attachments/assets/297ee50b-6536-4556-8915-7957c622f3b1)
![table](https://github.com/user-attachments/assets/a662f60f-554d-48d9-8ed7-c337ac966f4b)


## Included Utilities

### 1. Color Viewer
Browse through the Astrocade color palette eight colors at a time.

**Features**
- Displays eight consecutive colors per page.
- Joystick cycles pages.
- Knob allows fast color jumps with a trigger interlock.

### 2. Color Matcher
Compare and adjust multiple colors at once.

**Features**
- Four independent color rows.
- Select a row and adjust its color using the joystick.
- Knob allows fast cycling, also with trigger interlock.

### 3. Color Table (New)
Displays all 64 colors per page in a large grid with an independently adjustable background.

**Features**
- Displays full 64-color tables per page.
- Joystick left and right moves between pages. Pulling the trigger limits movement to a single column shift.
- Joystick up and down changes the background color.
- Knob provides fast background color cycling.
- Very helpful for choosing contrasting foreground and background colors on real hardware.

## Controls

### Color Viewer
- **Joystick Left/Right**: Cycle through sets of eight colors.
- **Knob**: Fast color shift (trigger required).

### Color Matcher
- **Joystick Up/Down**: Select which of the four rows is active.
- **Joystick Left/Right**: Change the color of the active row.
- **Knob**: Fast color shift for the active row (trigger required).

### Color Table
- **Joystick Left/Right**: Move between color grid pages.
- **Trigger + Left/Right**: Constrain page movement to one column.
- **Joystick Up/Down**: Change the background color.
- **Knob + Trigger**: Fast background color cycling.

## Bally Astrocade Color Palette

A reference palette is available at the Bally Alley site:  
https://ballyalley.com/ml/ml_docs/astrocade_palette.html  
These colors come from MAME and do not precisely match a real Astrocade. The utilities in this repo are designed to help choose colors on actual hardware.

<img alt="mame vs astrocade" src="https://github.com/user-attachments/assets/a58c96fc-43ea-4a60-ad1b-5589cb929dd5" />


## Development

- Developed with **Visual Studio Code** and the **MAME debugger**.
- Assembled with **Zmac 1.3**.

## License

This project is licensed under the MIT License.

## Acknowledgments

Thanks to Bally Alley for its extensive documentation and support for the Astrocade developer community:  
https://ballyalley.com/ml/ml_homebrew/ml_homebrew.html

---

### Assembly Instructions

To assemble the program, use:

```bash
zmac -i -o colors.bin colors.asm
