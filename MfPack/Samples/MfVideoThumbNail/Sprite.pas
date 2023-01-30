// FactoryX
//
// Copyright: © FactoryX. All rights reserved.
//
// Project: MfPack - Shared
// Project location: https://sourceforge.net/projects/MFPack
//                   https://github.com/FactoryXCode/MfPack
// Module: Sprite.pas
// Kind: Pascal / Delphi unit
// Release date: 08-07-2012
// Language: ENU
//
// Revision Version: 3.1.4
// Description: Videothumbnail sprite.
//
// Organisation: FactoryX
// Initiator(s): Tony (maXcomX), Peter (OzShips)
// Contributor(s): Tony Kalf (maXcomX), Peter Larson (ozships)
//
//------------------------------------------------------------------------------
// CHANGE LOG
// Date       Person              Reason
// ---------- ------------------- ----------------------------------------------
// 28/08/2022 All                 PiL release  SDK 10.0.22621.0 (Windows 11)
//------------------------------------------------------------------------------
//
// Remarks: Requires Windows 7 or higher.
//
// Related objects: -
// Related projects: MfPackX314
// Known Issues: -
//
// Compiler version: 23 up to 35
// SDK version: 10.0.20348.0
//
// Todo: -
//
//==============================================================================
// Source: Microsoft samples.
//
// Copyright (c) Microsoft Corporation. All rights reserved.
//==============================================================================
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// https://www.mozilla.org/en-US/MPL/2.0/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// Non commercial users may distribute this sourcecode provided that this
// header is included in full at the top of the file.
// Commercial users are not allowed to distribute this sourcecode as part of
// their product.
//
//==============================================================================
unit Sprite;

interface

uses
  {WinApi}
  Winapi.Messages,
  WinApi.WinApiTypes,
  {System}
  System.Classes,
  System.Types,
  System.Math,
  System.SysUtils,
  {DirectX}
  WinApi.DirectX.DCommon,
  WinApi.DirectX.D2D1,
  WinApi.DirectX.D2D1Helper,
  {MediaFoundationApi}
  WinApi.MediaFoundationApi.MfUtils,
  {Project}
  VideoTumbNailHelpers;


const
  WOBBLE_ANGLE = 10.0;
  WOBBLE_DECAY = 0.25;

  // from float.h
  FLT_EPSILON  = 1.19209290E-07; // smallest such that 1.0 + FLT_EPSILON >= 1.0

  MAX_SPRITES = 5; // You can have virtually as much sprites as you want.


type

  TFormatInfo = record
  public
    imageWidthPels: UINT32;
    imageHeightPels: UINT32;
    bTopDown: BOOL;
    rcPicture: TRect;     // Corrected for pixel aspect ratio

    procedure SetRectEmpty();
  end;

  TState = (_CLEAR,
            _PENDING,
            _BITMAP);
type

  TSprite = class(TObject)
  private

    m_pBitmap: ID2D1Bitmap;
    m_mat: D2D1_MATRIX_3X2_F;

    m_bTopDown: Boolean;
    m_AspectRatio: D2D1_SIZE_F;

    m_nrcBound: D2D_RECT_F;      // Bounding box, as a normalized rectangle.
    m_fill: D2D_RECT_F;          // Actual fill rectangle in pixels.

    // Animation
    m_bAnimating: Boolean;
    m_timeStart: FLOAT;          // Start time for the animation
    m_timeEnd: FLOAT;            // Ending time.
    m_nrcAnimDistance: D2D_RECT_F;    // Animation path.
    m_nrcStartPosition: D2D_RECT_F;   // Equal to m_nrcBound at m_timeStart.

    // Animation path is defined as follows:
    // Final bounding box = m_nrcBound + m_nrcAnimDistance

    // Wobble parameters
    m_fAngle: FLOAT;
    m_theta: FLOAT;

  protected

    // State:
    // Currently two states are defined, BITMAP and CLEAR, simply reflecting
    // whether the sprite contains a bitmap or not.

    function GetState(): TState;

  public
    // Sprite index
    m_SpriteIndex: int64;

    //Constructor & Destructor
    //Sprite();
    constructor Create();
    //~Sprite();
    destructor Destroy(); override;


    procedure SetBitmap(pBitmap: ID2D1Bitmap;
                        const pformat: TFormatInfo);

    procedure AnimateBoundingBox(bound2: D2D_RECT_F;
                                 const ftime: FLOAT;
                                 const duration: FLOAT);

    procedure Update(pRT: ID2D1HwndRenderTarget;
                     ftime: FLOAT);

    procedure Draw(pRT: ID2D1HwndRenderTarget);

    function HitTest(const x: Integer;
                     const y: Integer): Boolean;

    procedure Clear();

end;

  TSpritesArray = array [0..MAX_SPRITES] of TSprite;


implementation



procedure TFormatInfo.SetRectEmpty();
begin
  Self := Default(TFormatInfo);
end;



// TSprite
//========


//-------------------------------------------------------------------
// Sprite constructor
//-------------------------------------------------------------------
//Sprite();
constructor TSprite.Create();
begin
  m_pBitmap := nil;
  m_bAnimating := FALSE;
  m_timeStart := 0;
  m_timeEnd := 0;
  m_fAngle := 0;
  m_theta := 0;
  m_bTopDown := False;
end;

//-------------------------------------------------------------------
// Sprite destructor
//-------------------------------------------------------------------
//~Sprite();
destructor TSprite.Destroy();
begin
  if Assigned(m_pBitmap) then
    begin
      SafeRelease(m_pBitmap);
    end;
end;


function TSprite.GetState(): TState;
begin
  if Assigned(m_pBitmap) then
    Result := _BITMAP
  else
    Result := _CLEAR;
end;


//-------------------------------------------------------------------
// SetBitmap: Sets the bitmap for the sprite.
//-------------------------------------------------------------------
procedure TSprite.SetBitmap(pBitmap: ID2D1Bitmap;
                            const pformat: TFormatInfo);
begin
  SafeRelease(m_pBitmap);

  if Assigned(pBitmap) then
    begin
      m_pBitmap := pBitmap;
      //m_pBitmap._AddRef();   // >> Delphi takes care of that.
    end;

    m_bTopDown := pformat.bTopDown;

    D2D1RectFDefault(m_nrcBound);  // set rect
    D2D1RectFDefault(m_fill);

    m_AspectRatio := D2D1SizeF(pformat.rcPicture.right,
                               pformat.rcPicture.bottom);
end;

//-------------------------------------------------------------------
// AnimateBoundingBox
//
// Applies an animation path to the sprite.
//
// bound2: Final position of the bounding box, as a normalized rect.
// time: Current clock time.
// duration: Length of the animation, in seconds.
//-------------------------------------------------------------------
procedure TSprite.AnimateBoundingBox(bound2: D2D_RECT_F;
                                     const fTime: FLOAT;
                                     const duration: FLOAT);
begin
  if (duration = 0.0) then
    begin
        // Apply the new position immediately

        m_nrcBound := bound2;
        m_bAnimating := FALSE;
        m_fAngle := 0.0;
    end
  else
    begin
        // Set the animation parameters.

        m_timeStart := fTime;
        m_timeEnd := fTime + duration;

        m_nrcAnimDistance := SubtD2D1RectF(bound2,
                                           m_nrcBound);

        m_nrcStartPosition := m_nrcBound;

        m_fAngle := WOBBLE_ANGLE;

        m_bAnimating := TRUE;
    end;
end;

//-------------------------------------------------------------------
// Update: Updates the sprite, based on the clock time.
//-------------------------------------------------------------------
procedure TSprite.Update(pRT: ID2D1HwndRenderTarget;
                         fTime: FLOAT);
var
  v: D2D1_RECT_F;
  sizeBitmap: D2D1_SIZE_F;
  sizeRT: D2D1_SIZE_F;
  _rect: D2D1_RECT_F;

begin

  if (GetState() = _CLEAR) then
    begin
      Exit; // No bitmap; nothing to do.
    end;

  if ((m_timeStart < fTime) AND (m_timeEnd > fTime)) then
    begin
        // We are in the middle of an animation. Interpolate the position.
        v := multD2D1RectFxF(m_nrcAnimDistance,
                            ((fTime - m_timeStart) / (m_timeEnd - m_timeStart)));
        m_nrcBound := SumD2D1RectF(v,
                                   m_nrcStartPosition);
    end
  else if (m_bAnimating AND (fTime >= m_timeEnd)) then
    begin
        // We have reached the end of an animation.
        // Set the final position (avoids any rounding errors)
        m_nrcBound := SumD2D1RectF(m_nrcStartPosition,
                                   m_nrcAnimDistance);
        m_bAnimating := FALSE;
    end;

  // Compute the correct letterbox for the bitmap.
  //
  // TODO: Strictly, this only needs to be updated if the bitmap changes
  //       or the size of the render target changes.

  sizeBitmap := m_AspectRatio;

  pRT.GetSize(sizeRT);

  _rect.right := WidthD2D1RectF(m_nrcBound) * sizeRT.width;
  _rect.bottom := HeightD2D1RectF(m_nrcBound) * sizeRT.height;

  m_fill := LetterBoxRectF(sizeBitmap,
                           _rect);
end;

//-------------------------------------------------------------------
// Draw: Draws the sprite.
//-------------------------------------------------------------------
procedure TSprite.Draw(pRT: ID2D1HwndRenderTarget);
var
  sizeRT: D2D1_SIZE_F;
  _width, _height: FLOAT;
  mat: D2D1_MATRIX_3X2_F;


  // debug
  //iSpriteIndex: Integer;

begin

  // debug: check sprite index
  //iSpriteIndex := m_SpriteIndex;

  if (GetState() = _CLEAR) then
    begin
      Exit; // No bitmap; nothing to do.
    end;

  pRT.GetSize(sizeRT);

  _width := WidthD2D1RectF(m_nrcBound) * sizeRT.width;
  _height := HeightD2D1RectF(m_nrcBound) * sizeRT.height;

  // Start with an identity transform.
  mat := D2D1_MATRIX_3X2_F.Identity();


  // If the image is bottom-up, flip around the x-axis.
  if (m_bTopDown = FALSE) then
    begin
      mat := D2D1_MATRIX_3X2_F.Scale(D2D1SizeF(1, -1),
                                     D2D1PointF(0, (_height / 2)));
    end;

  // Apply wobble.
  if (m_fAngle >= FLT_EPSILON) then
    begin
      mat := mat * D2D1_MATRIX_3X2_F.Rotation((m_fAngle * sin(m_theta)),
                                              D2D1PointF((_width / 2), (_height / 2 )));

      // Reduce the wobble by the decay factor...
      m_theta := m_theta + WOBBLE_DECAY;

      m_fAngle := m_fAngle - WOBBLE_DECAY;

      if (m_fAngle <= FLT_EPSILON) then
        begin
          m_fAngle := 0.0;
        end
    end;

  // Now translate the image relative to the bounding box.
  mat := mat * D2D_MATRIX_3X2_F.Translation((m_nrcBound.left * sizeRT.width),
                                            (m_nrcBound.top * sizeRT.height));

  pRT.SetTransform(mat);

  // Store the matrix
  m_mat := mat;

  //
  pRT.DrawBitmap(m_pBitmap,
                 @m_fill);

end;

//-------------------------------------------------------------------
// HitTest: Returns true if (x, y) falls within the bitmap.
//-------------------------------------------------------------------
function TSprite.HitTest(const x: Integer;
                         const y: Integer): Boolean;
var
  mat: D2D_MATRIX_3X2_F;
  pt: D2D1_POINT_2F;

begin
  mat := m_mat;

  // Use the inverse of our current transform matrix to transform the
  // point (x,y) from render-target space to model space.

  mat.Invert();
  pt := D2DMatrix3X2FTransformPoint(mat,
                                    D2D1PointF(x,
                                               y));

  if (pt.x >= m_fill.left) AND (pt.x <= m_fill.right) AND (pt.y >= m_fill.top) AND (pt.y <= m_fill.bottom) then
    begin
      Result := True;
    end
  else
    begin
      Result := False;
    end
end;

//-------------------------------------------------------------------
// Clear: Clears the bitmap.
//-------------------------------------------------------------------
procedure TSprite.Clear();
begin
  SafeRelease(m_pBitmap);

  m_nrcBound := D2D1RectF(0.0,
                          0.0,
                          0.0,
                          0.0);
  m_fill := m_nrcBound;

  m_AspectRatio := D2D1SizeF(1.0,
                             1.0);
end;

end.
