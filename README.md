# Color Viewer & Color Matcher for Bally Astrocade

## Overview

**Color Viewer & Color Matcher** is a utility program designed for the Bally Astrocade, allowing users to cycle through and compare the console's color palette. The utility comprises two separate programs: a Color Viewer and a Color Matcher.

### Features

- **Color Viewer**: 
  - Browse through the Astrocade's color palette 8 consecutive colors at a time.
  - Use the joystick to jump to the previous or next set of 8 colors.
  - Utilize the knob for faster color cycling with a trigger interlock to prevent accidental shifts.

- **Color Matcher**: 
  - Display and compare up to 4 different colors simultaneously.
  - Adjust individual color rows using the joystick.
  - Quickly browse colors using the knob with a trigger interlock to avoid unintentional changes.

## Controls

### Color Viewer

- **Joystick Left/Right**: Cycle through colors 8 at a time.
- **Knob**: Faster color cycling (requires trigger pull).

### Color Matcher

- **Joystick Left/Right**: Change the color of the selected row.
- **Joystick Up/Down**: Select the row to modify.
- **Knob**: Faster color cycling for the selected row (requires trigger pull).

## Bally Astrocade Color Palette

For a detailed look at the Bally Astrocade color palette, visit [Bally Alley's Astrocade Palette Documentation](https://ballyalley.com/ml/ml_docs/astrocade_palette.html).  Please note that these colors are from the MAME emulator and don't precisely match the actual console colors.

## Development

- Developed using **Visual Studio Code** and the **MAME emulator/debugger**.
- Assembled with **Zmac 1.3**.

## License

This project is licensed under the MIT License.

## Acknowledgments

Grateful thanks to the wealth of information available at [Bally Alley](https://ballyalley.com/ml/ml_homebrew/ml_homebrew.html).

---

### Assembly Instructions

To assemble the program, use Zmac 1.3 with the following command:

```bash
zmac -i -o colors.bin colors.asm
