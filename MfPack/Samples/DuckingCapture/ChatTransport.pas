unit ChatTransport;

interface

uses
  WinApi.Windows,
  WinApi.Messages;

type

  ChatTransportType = (ChatTransportWave,
                       ChatTransportWasapi);

  //
  //  Pure virtual class which defines a "Chat Transport"
  //

  CChatTransport = class
  protected
    _AppWindow: HWND;

  public
    constructor Create(_hWnd: HWND); virtual; abstract;
    function Initialize(const UseCaptureDevice: Boolean): Boolean; virtual; abstract;
    procedure Shutdown(); virtual; abstract;

    function StartChat(const HideFromVolumeMixer: Boolean): Boolean; virtual; abstract;
    procedure StopChat(); virtual; abstract;

    function TransportType(): ChatTransportType; virtual; abstract;

    // We left out the C++ handling of messages, to keep things simple and do it the Delphi way.
  end;


implementation

end.
