# TMfGuiRefreshTimer

`TMfGuiRefreshTimer` is a nonvisual Delphi XE7+ VCL component for periodic GUI
refreshes. All instances share one worker thread and one hidden message window.
The worker posts a custom message instead of using `WM_TIMER`.

Install the updated `MfComponents.dpk` to find it on the **MfPack Timer Samples**
palette. The published properties follow the usual timer interface:

| Property | Default | Meaning |
|---|---:|---|
| `Enabled` | `True` | Enables scheduling when an event is assigned. |
| `Interval` | `1000` | Milliseconds between completed callbacks; `0` suspends scheduling. |
| `OnTimer` | Unassigned | Event executed on the VCL main thread. |

Use the form designer to drop the component and assign its event as with a
standard `TTimer`. It does not start a worker or fire events in design mode.

```pascal
procedure TMyForm.GuiRefreshTimerTimer(Sender: TObject);
begin

  lblPlayed.Caption := FormatPlayedTime(FPlayedSnapshot);
end;
```

For code-created components, set properties on the main thread:

```pascal
  FRefreshTimer := TMfGuiRefreshTimer.Create(Self);
  FRefreshTimer.Enabled := False;
  FRefreshTimer.Interval := 40;
  FRefreshTimer.OnTimer := RefreshDisplay;
  FRefreshTimer.Enabled := True;
```

## Scheduling and lifetime

- There is at most one pending scheduler message for all instances together.
  A blocked GUI receives the latest refresh once; missed ticks are not replayed.
- Callbacks are not reentered, including when a handler pumps messages or opens
  a modal dialog. The next interval begins when that callback finishes.
- A handler may disable/free its timer or another registered timer. Registration
  IDs are checked again before invoking each handler. Messages contain no timer
  object pointers.
- Create, configure and destroy these components on the main thread. Event
  handlers may access VCL controls. The worker never touches controls and never
  calls `Synchronize`.
- Callback exceptions go through `Application.HandleException`; the registration
  remains usable unless the handler disabled or freed it.
- The worker sleeps when there is no scheduled work, and waits while a refresh
  is already pending. It is released when the unit is finalized. Its shutdown
  does not depend on the main thread pumping messages.
- No timer-resolution changes or multimedia/audio priority boosts are requested.

This is a GUI scheduler, not a real-time audio clock. Keep handlers short: copy
the latest engine values and invalidate/update controls. Decode, capture,
recording and network requests belong on their existing workers. A blocked main
thread still delays callbacks, and slow paint handlers still need optimization.

Typical display intervals are 33–50 ms for moving indicators and 1000 ms for a
clock. Fewer shared refreshes are preferable to a separate high-frequency timer
for every control. Replacing existing peak-meter timers is a separate migration.

## Verification

Run `Tests/Run-GuiRefreshTimerTests.py` with the bundled or another Python 3
interpreter. `--delphi-root` selects the Delphi installation; the default is XE7.
Both Win32 and Win64 tests cover one worker shared by 64 timers, main-thread
callbacks, a continuously busy posted-message queue, coalescing after a blocked
GUI, disable/free while pending, self/sibling deletion during dispatch, nested
message pumping, exception recovery, streaming, design-time inactivity and
1000 rapid lifetime changes. No audio devices or user settings are used.
