object Form1: TForm1
  Left = 0
  Top = 0
  Width = 479
  Height = 355
  AutoScroll = True
  Caption = 'TR4 to prj'
  Color = clBtnFace
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
  object Image1: TImage
    Left = 88
    Top = 8
    Width = 256
    Height = 256
    AutoSize = True
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 277
    Width = 463
    Height = 19
    Panels = <>
    ExplicitTop = 223
    ExplicitWidth = 472
  end
  object ActionList1: TActionList
    Left = 424
    Top = 8
    object FileOpen1: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Dialog.DefaultExt = '.tr4'
      Dialog.FileName = 'mix1w'
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
  end
  object MainMenu1: TMainMenu
    Left = 368
    Top = 8
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
end
