object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TR4 to PRJ'
  ClientHeight = 296
  ClientWidth = 463
  Color = clBtnFace
  Constraints.MinHeight = 355
  Constraints.MinWidth = 479
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge1: TGauge
    Left = 296
    Top = 243
    Width = 159
    Height = 23
    Progress = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 277
    Width = 463
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 272
  end
  object Button1: TButton
    Left = 296
    Top = 8
    Width = 145
    Height = 41
    Action = FileOpen2
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 282
    Height = 277
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 0
      Width = 282
      Height = 277
      HorzScrollBar.Visible = False
      Align = alClient
      TabOrder = 0
      OnMouseWheel = ScrollBox1MouseWheel
      object Image1: TImage
        Left = -2
        Top = -2
        Width = 256
        Height = 256
        AutoSize = True
        PopupMenu = PopupMenu1
      end
    end
  end
  object Button2: TButton
    Left = 296
    Top = 55
    Width = 145
    Height = 41
    Action = UnloadPRJ
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 296
    Top = 102
    Width = 145
    Height = 91
    Caption = 'Copy from TR2PRJ'
    TabOrder = 4
    object CheckBox1: TCheckBox
      Left = 16
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Doors'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Textures'
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 62
      Width = 97
      Height = 17
      Caption = 'Lights'
      TabOrder = 2
    end
  end
  object ActionList1: TActionList
    Left = 384
    Top = 192
    object FileOpen1: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Dialog.DefaultExt = '.tr4'
      Dialog.Filter = 'Tomb Raider 4 Files (*.tr4)|*.tr4|All files (*.*)|*.*'
      Dialog.Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
      OnAccept = FileOpen1Accept
    end
    object FileExit1: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
      ShortCut = 27
    end
    object FileSaveAs1: TFileSaveAs
      Category = 'File'
      Caption = 'Save &As...'
      Dialog.DefaultExt = 'prj'
      Dialog.Filter = 'TRLE Project Files (*.prj)|*.prj'
      Dialog.Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
      Hint = 'Save As|Saves the active file with a new name'
      ImageIndex = 30
      BeforeExecute = FileSaveAs1BeforeExecute
      OnAccept = FileSaveAs1Accept
      OnUpdate = FileSaveAs1Update
    end
    object FileOpen2: TFileOpen
      Category = 'File'
      Caption = 'Load &TR2PRJ project...'
      Dialog.DefaultExt = 'prj'
      Dialog.Filter = 'TRLE project Files (*.prj)|*.prj|All Files (*.*)|*.*'
      Dialog.Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
      OnAccept = FileOpen2Accept
      OnUpdate = FileOpen2Update
    end
    object UnloadPRJ: TAction
      Caption = 'Unload TR2PRJ project'
      Hint = 'Unload a loaded PRJ'
      OnExecute = UnloadPRJExecute
      OnUpdate = UnloadPRJUpdate
    end
  end
  object MainMenu1: TMainMenu
    Left = 424
    Top = 192
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Action = FileOpen1
      end
      object SaveAs1: TMenuItem
        Action = FileSaveAs1
      end
      object Exit1: TMenuItem
        Action = FileExit1
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'tga'
    Filter = 'TrueVision Targa Files (*.tga)|*.tga'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 296
    Top = 192
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 344
    Top = 192
    object SaveTGA1: TMenuItem
      Caption = 'Save TGA...'
      OnClick = SaveTGA1Click
    end
    object Cancel1: TMenuItem
      Caption = 'Cancel'
      OnClick = Cancel1Click
    end
  end
end
