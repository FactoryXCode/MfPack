unit MfPCXConstants;

interface

uses
  {WinApi}
  Winapi.Messages;

const

  // Message ID's
  WM_PARENTPOSCHANGED          = WM_APP + 100;
  WM_PARENTSIZECHANGED         = WM_APP + 101;
  WM_PROGRESSNOTIFY            = WM_APP + 102;
  WM_TIMERNOTIFY               = WM_APP + 103;
  WM_QUEUETIMERNOTIFY          = WM_APP + 104;
  WM_TIMEDTEXTNOTIFY_UPDATE    = WM_APP + 105;
  WM_TIMEDTEXTNOTIFY_INIT      = WM_APP + 106;
  WM_TIMEDTEXTNOTIFY_PROCESSED = WM_APP + 107;

  WM_PLAYER_EVENT              = WM_APP + 110;


  // Timed Textfile extensions.

  // Subtitle File Extensions
  EXTSUBRIP    = '.srt';  // SubRip
  EXTMICRODVD  = '.sub';  // MicroDvd / SubViewer

  // YouTube (not implemented)
  EXTYOUTUBE   = '.sbv';

  // SAMI file extensions (not implemented)
  EXTSMI       = '.smi';  // SAMI
  EXTSAMI      = '.sami'; // SAMI (rarely used)
  // About the SAMI Media Source
  // Synchronized Accessible Media Interchange (SAMI) is a format for adding captions to digital media.
  // The captions are stored in a separate text file with the file name extension .smi or .sami.
  //
  // In Media Foundation, SAMI caption files are supported through the SAMI media source (very strict, and there fore buggy).
  // Use the Source Resolver to create an instance of the SAMI media source from a URL or byte stream.
  // Media Foundation does not provide a component that displays SAMI captions.
  // The application must interpret the caption data that it receives from the SAMI media source.

  // WebVTT caption format for HTML5 media players. (not implemented)
  EXTWEBVTT    = '.vtt';


  TFLENGTH          = 8;

  LFEED             = #13;
  ULBR              = #13#10;
  LINEBR            = '<BR-';

  // SUB tags //////////////////////////////////////////////////////////////////

  // The timing tags have this format: {start-frame}{stop-frame} followed by text.
  // Example: {45679}{49805}Hello World.

  SUB_NEWLINE            = '|';    // New line Example: Line1.|Line2.
  SUB_FONT_BOLD          = '{y:b}';
  SUB_FONT_UNDERLINE     = '{y:u}';
  SUB_FONT_ITALIC        = '{y:i}';
  SUB_FONT_FONT          = '{f:';  // Example: {f:System}
  SUB_FONT_SIZE          = '{s:';  // Example: {s:12}
  SUB_FONT_COLOR         = '{c:$'; // Example: {c:$0000FF}. Format: $BBGGRR (8 Blue, 8 Green and 8 Red bits)
  SUB_FONT_POSITION      = '{p:';  // Example: {P:X,Y}
  SUB_DEFAULT            = '{DEFAULT}'; // Default for plain texttags. {DEFAULT}{C:$0000FF}{F:Arial}{S:12}

  //////////////////////////////////////////////////////////////////////////////


  // SRT tags //////////////////////////////////////////////////////////////////

  WTP                    = '-->';   // srt time separator
  TIMECODELENGTH         = 12;      // Length of the srt-timecode

  // SRT (SubRip) supports the following font tags
  SRTTAG_ITALIC_START    = '<i>';
  SRTTAG_ITALIC_END      = '</i>';
  SRTTAG_BOLD_START      = '<b>';
  SRTTAG_BOLD_END        = '</b>';
  SRTTAG_UNDERLINE_START = '<u>';
  SRTTAG_UNDERLINE_END   = '</u>';
  SRTTAG_COLOR_START     = '<font color="';
  SRTTAG_COLOR_END       = '</font>';
  SRTTAG_COLOR_EQOUTE    = '">';

  //////////////////////////////////////////////////////////////////////////////


  // WEBVTT tags (not implemented) /////////////////////////////////////////////
  WVTT_Mime            = 'text/vtt';
  WVTT_Kind            = 'WEBVTT Kind: ';
  WVTT_Lang            = 'Language: ';
  WVTT_HeadTxt         = 'WEBVTT - ';
  WVTT_Note            = 'NOTE ';
  WVTT_Style           = 'STYLE ';
  WVTT_TimeSep         = ' --> ';
  WVTT_Voice_Start     = '<v ';
  WVTT_Voice_End       = '/v';
  WVTT_EndTag          = '>';
  WVTT_Cue             = '::cue';     // Syntax: ::cue(#\31) { color: lime; }
  WVTT_Cs              = '(';
  WVTT_Ce              = ')';
  WVTT_Align           = 'align:';    // Syntax: align:start .. align:middle .. align:end
  WVTT_Size            = 'size:';     // Syntax: size:50%
  WVTT_Position        = 'position:'; // Syntax: position:72%
  WVTT_Vertical        = 'vertical:';
  WVTT_Line            = 'line:';     // Syntax: line:63%
  WVTT_Class_Start     = '<c>';
  WVTT_Class_End       = '</c>';
  WVTT_Lang_Start      = '<lang ';    // Syntax: <lang en>English</lang>
  WVTT_Lang_End        = '</lang>';
  WVTT_Color           = '{ color:';  // Syntax: { color: lime; }, { color: rgb(0 255 0); }, { color: rgb(0% 100% 0%); }
  WVTT_Ae              = ' }';
  WVTT_ClColor         = '<c.';       // Syntax: <c.yellow.bg_blue> This is yellow text on a blue background </c>

  // Font tags
  WVTT_ITALIC_START    = '<i>';
  WVTT_ITALIC_END      = '</i>';
  WVTT_BOLD_START      = '<b>';
  WVTT_BOLD_END        = '</b>';
  WVTT_UNDERLINE_START = '<u>';
  WVTT_UNDERLINE_END   = '</u>';
  WVTT_RUBY_START      = '<ruby>';
  WVTT_RUBY_END        = '</ruby>';

  //////////////////////////////////////////////////////////////////////////////

  // Aspectratio's
  //////////////////////////////////////////////////////////////////////////////
  // Television
  AR_4_3   : Single = 1.333333333333333;
  AR_5_3   : Single = 1.666666666666667;
  // HDTV / WideScreen
  AR_16_9  : Single = 1.777777777777778;
  // Mobile phones
  AR_9_16  : Single = 0.5625;
  AR_18_9  : Single = 2.0;   // Univisium format: Google Pix, LG, Huawei etc.
  AR_195_9 : Single = 2.16;  // Apple, Samsung
  // Cinema
  AR_186_1 : Single = 1.86;
  AR_21_9  : Single = 2.37037037037037; // = 64:27 !
  AR_235_1 : Single = 2.35; // European cinema format
  AR_239_1 : Single = 2.39;
  AR_24_1  : Single = 2.4;
  //////////////////////////////////////////////////////////////////////////////


implementation

end.
