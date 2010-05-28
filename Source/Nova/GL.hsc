{-# OPTIONS -cpp #-}
{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, TemplateHaskell #-}
{-
	GL.hsc
	Nibbana

	Created by Lyndon Tremblay on 11/01/07.
	Copyright 2007 Hoovy Studios. All rights reserved.
-}

module Nova.GL where

#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#endif

#ifndef CALLCONV
#define CALLCONV ccall
#endif

import Foreign.Ptr
import Data.Word
import Data.Int
import Foreign.Marshal
import Foreign.Storable

type GLenum     = #{type GLenum}
type GLbitfield = #{type GLbitfield}
type GLboolean  = #{type GLboolean}
type GLsizei    = #{type GLsizei}
type GLfloat    = #{type GLfloat}
type GLdouble   = #{type GLdouble}
type GLint      = #{type GLint}
type GLuint     = #{type GLuint}
--type GLsizeiptr = #{type GLsizeiptr}
type GLclampd   = #{type GLclampd}
type GLclampf   = #{type GLclampf}

--glFog = #{const GL_FOG}

class ToConst a b | a -> b where
    toConst :: a -> b

class FromConst a b | a -> b where
    fromConst :: b -> a

instance ToConst Bool GLboolean where
    toConst  True = #{const  GL_TRUE}
    toConst False = #{const GL_FALSE}

instance FromConst Bool GLboolean where
    fromConst #{const GL_TRUE}  = True
    fromConst #{const GL_FALSE} = False

data Constants =
      Fog | Lighting | Texture1D | Texture2D | Blend
    | LineStipple | CullFace | AlphaTest
    | StencilTest | DepthTest | ScissorTest
    | PointSmooth | LineSmooth --  | PolygonSmooth
--  | ColorMaterial
--  | Normalize | AutoNormal
--  | VertexArray | NormalArray | ColorArray | IndexArray | TextureCoordArray
--  | PolygonOffsetPoint
--  | PolygonOffsetLine
--  | PolygonOffsetFill
    | Flat | Smooth | Clockwise | CounterClockwise
    -- attrib mask
    | DepthBufferBit | AccumBufferBit | StencilBufferBit | ColorBufferBit
    | ViewportBit | LineBit | PointBit | EnableBit | TransformBit | CurrentBit
    -- begin mode
    | Points | Lines | LineLoop | LineStrip
    | Triangles | TriangleStrip | TriangleFan
    | Quads | QuadStrip | Polygon
    -- matrix mode
    | Modelview | Projection | Texture 
    -- polygon mode
    | Point | Line | Fill
    -- face mode
    | Front | Back | FrontAndBack | Aux0 | Aux1
    --  | FramebufferEXT | RenderbufferEXT | StencilAttachmentEXT | ColorAttachment0EXT | DepthAttachmentEXT | FramebufferCompleteEXT
    -- internal format
    | RGB | RGB8 | RGBA | RGBA8 | UnsignedByte | Byte | DepthComponent
    -- blend / depth
    | Zero | One | DestColor | SourceColor | OneMinusDestColor | SourceAlpha | OneMinusSourceAlpha
    | DestAlpha | OneMinusDestAlpha --  | DestAlphaSaturate
    | Never | Less | Equal | LEqual | Greater | NotEqual | GEqual | Always
    -- texture params
    | Nearest | TextureMagFilter | TextureMinFilter
    -- buffers
{-
    | ArrayBuffer | ElementArrayBuffer | PixelPackBuffer | PixelUnpackBuffer
    | StreamDraw | StreamRead | StreamCopy | StaticDraw | StaticRead | StaticCopy
    | DynamicDraw | DynamicRead | DynamicCopy | ReadOnly | WriteOnly | ReadWrite
-}
  deriving (Eq,Show,Read)

{-
GL_ALPHA, GL_ALPHA4,
			  GL_ALPHA8, GL_ALPHA12, GL_ALPHA16,
			  GL_LUMINANCE,	GL_LUMINANCE4, GL_LUMINANCE8,
			  GL_LUMINANCE12, GL_LUMINANCE16,
			  GL_LUMINANCE_ALPHA, GL_LUMINANCE4_ALPHA4,
			  GL_LUMINANCE6_ALPHA2,	GL_LUMINANCE8_ALPHA8,
			  GL_LUMINANCE12_ALPHA4,
			  GL_LUMINANCE12_ALPHA12,
			  GL_LUMINANCE16_ALPHA16, GL_INTENSITY,
			  GL_INTENSITY4, GL_INTENSITY8,
			  GL_INTENSITY12, GL_INTENSITY16, GL_RGB,
			  GL_R3_G3_B2, GL_RGB4,	GL_RGB5, GL_RGB8,
			  GL_RGB10, GL_RGB12, GL_RGB16,	GL_RGBA,
			  GL_RGBA2, GL_RGBA4, GL_RGB5_A1, GL_RGBA8,
			  GL_RGB10_A2, GL_RGBA12, or GL_RGBA16.
-}

-- GL_COLOR, GL_DEPTH, GL_STENCIL

instance ToConst Constants GLenum where
    toConst Fog              = #{const GL_FOG}
    toConst Lighting         = #{const GL_LIGHTING}
    toConst Texture1D        = #{const GL_TEXTURE_1D}
    toConst Texture2D        = #{const GL_TEXTURE_2D}
    toConst DepthTest        = #{const GL_DEPTH_TEST}
    toConst CullFace         = #{const GL_CULL_FACE}
    toConst LineStipple      = #{const GL_LINE_STIPPLE}
    toConst PointSmooth      = #{const GL_POINT_SMOOTH}
    toConst LineSmooth       = #{const GL_LINE_SMOOTH}
    toConst StencilTest      = #{const GL_STENCIL_TEST}
    toConst ScissorTest      = #{const GL_SCISSOR_TEST}
    toConst AlphaTest        = #{const GL_ALPHA_TEST}
    toConst Blend            = #{const GL_BLEND}
    toConst Flat             = #{const GL_FLAT}
    toConst Smooth           = #{const GL_SMOOTH}
    toConst Clockwise        = #{const GL_CW}
    toConst CounterClockwise = #{const GL_CCW}
-- attrib mask
    toConst ColorBufferBit   = #{const GL_COLOR_BUFFER_BIT}
    toConst DepthBufferBit   = #{const GL_DEPTH_BUFFER_BIT}
    toConst AccumBufferBit   = #{const GL_ACCUM_BUFFER_BIT}
    toConst StencilBufferBit = #{const GL_STENCIL_BUFFER_BIT}
    toConst ViewportBit      = #{const GL_VIEWPORT_BIT}
    toConst EnableBit        = #{const GL_ENABLE_BIT}
    toConst TransformBit     = #{const GL_TRANSFORM_BIT}
    toConst LineBit          = #{const GL_LINE_BIT}
    toConst PointBit         = #{const GL_POINT_BIT}
    toConst CurrentBit       = #{const GL_CURRENT_BIT}
    -- begin mode
    toConst Points        = #{const GL_POINTS}
    toConst Lines         = #{const GL_LINES}
    toConst LineLoop      = #{const GL_LINE_LOOP}
    toConst LineStrip     = #{const GL_LINE_STRIP}
    toConst Triangles     = #{const GL_TRIANGLES}
    toConst TriangleFan   = #{const GL_TRIANGLE_FAN}
    toConst TriangleStrip = #{const GL_TRIANGLE_STRIP}
    toConst Quads         = #{const GL_QUADS}
    toConst QuadStrip     = #{const GL_QUAD_STRIP}
    toConst Polygon       = #{const GL_POLYGON}
    -- matrix mode
    toConst Modelview     = #{const GL_MODELVIEW}
    toConst Projection    = #{const GL_PROJECTION}
    toConst Texture       = #{const GL_TEXTURE}
    -- polygon mode
    toConst Point = #{const GL_POINT}
    toConst Line  = #{const GL_LINE}
    toConst Fill  = #{const GL_FILL}
    -- face mode
    toConst Front        = #{const GL_FRONT}
    toConst Back         = #{const GL_BACK}
    toConst FrontAndBack = #{const GL_FRONT_AND_BACK}
    toConst Aux0         = #{const GL_AUX0}
    toConst Aux1         = #{const GL_AUX1}
{-
    toConst FramebufferEXT = #{const GL_FRAMEBUFFER_EXT}
    toConst RenderbufferEXT = #{const GL_RENDERBUFFER_EXT}
    toConst ColorAttachment0EXT = #{const GL_COLOR_ATTACHMENT0_EXT}
    toConst DepthAttachmentEXT = #{const GL_DEPTH_ATTACHMENT_EXT}
    toConst StencilAttachmentEXT = #{const GL_STENCIL_ATTACHMENT_EXT}
    toConst FramebufferCompleteEXT = #{const GL_FRAMEBUFFER_COMPLETE_EXT}
-}
    -- internal format
    toConst RGB            = #{const GL_RGB}
    toConst RGB8           = #{const GL_RGB8}
    toConst RGBA           = #{const GL_RGBA}
    toConst RGBA8          = #{const GL_RGBA8}
    toConst UnsignedByte   = #{const GL_UNSIGNED_BYTE}
    toConst Byte           = #{const GL_BYTE}
    toConst DepthComponent = #{const GL_DEPTH_COMPONENT}
    -- blend / depth
    toConst Zero                = #{const GL_ZERO}
    toConst One                 = #{const GL_ONE}
    toConst DestColor           = #{const GL_DST_COLOR}
    toConst SourceColor         = #{const GL_SRC_COLOR}
    toConst OneMinusDestColor   = #{const GL_ONE_MINUS_DST_COLOR}
    toConst SourceAlpha         = #{const GL_SRC_ALPHA}
    toConst OneMinusSourceAlpha = #{const GL_ONE_MINUS_SRC_ALPHA}
    toConst DestAlpha           = #{const GL_DST_ALPHA}
    toConst OneMinusDestAlpha   = #{const GL_ONE_MINUS_DST_ALPHA}
    toConst Never    = #{const GL_NEVER}
    toConst Less     = #{const GL_ALWAYS}
    toConst Equal    = #{const GL_EQUAL}
    toConst LEqual   = #{const GL_LEQUAL}
    toConst Greater  = #{const GL_GREATER}
    toConst NotEqual = #{const GL_NOTEQUAL}
    toConst GEqual   = #{const GL_GEQUAL}
    toConst Always   = #{const GL_ALWAYS}
    -- texture params
    toConst Nearest          = #{const GL_NEAREST}
    toConst TextureMagFilter = #{const GL_TEXTURE_MAG_FILTER}
    toConst TextureMinFilter = #{const GL_TEXTURE_MIN_FILTER}
    -- buffers
{-
    toConst ArrayBuffer = #{const GL_ARRAY_BUFFER}
    toConst ElementArrayBuffer = #{const GL_ELEMENT_ARRAY_BUFFER}
    toConst PixelUnpackBuffer = #{const GL_PIXEL_UNPACK_BUFFER}
    toConst PixelPackBuffer = #{const GL_PIXEL_PACK_BUFFER}
    toConst ReadOnly = #{const GL_READ_ONLY}
    toConst WriteOnly = #{const GL_WRITE_ONLY}
    toConst ReadWrite = #{const GL_READ_WRITE}
-}

instance FromConst Constants GLenum where
    fromConst #{const GL_FOG} = Fog
    fromConst #{const GL_DEPTH_TEST} = DepthTest
    fromConst #{const GL_CULL_FACE} = CullFace
    fromConst #{const GL_TEXTURE_1D} = Texture1D
    fromConst #{const GL_TEXTURE_2D} = Texture2D
    fromConst #{const GL_LIGHTING} = Lighting
    fromConst #{const GL_LINE_STIPPLE} = LineStipple
    fromConst #{const GL_POINT_SMOOTH} = PointSmooth
    fromConst #{const GL_LINE_SMOOTH} = LineSmooth
    fromConst #{const GL_STENCIL_TEST} = StencilTest
    fromConst #{const GL_SCISSOR_TEST} = ScissorTest
    fromConst #{const GL_ALPHA_TEST} = AlphaTest
    fromConst #{const GL_BLEND} = Blend
    -- attrib mask
    fromConst #{const GL_COLOR_BUFFER_BIT} = ColorBufferBit
    fromConst #{const GL_DEPTH_BUFFER_BIT} = DepthBufferBit
    fromConst #{const GL_ACCUM_BUFFER_BIT} = AccumBufferBit
    fromConst #{const GL_STENCIL_BUFFER_BIT} = StencilBufferBit
    fromConst #{const GL_VIEWPORT_BIT} = ViewportBit
    fromConst #{const GL_ENABLE_BIT} = EnableBit
    -- matrix mode
    fromConst #{const GL_MODELVIEW} = Modelview
    fromConst #{const GL_PROJECTION} = Projection
    fromConst #{const GL_TEXTURE} = Texture
    -- polygon mode
    fromConst #{const GL_POINT} = Point
    fromConst #{const GL_LINE} = Line
    fromConst #{const GL_FILL} = Fill
    -- face mode
    fromConst #{const GL_FRONT} = Front
    fromConst #{const GL_BACK} = Back
    fromConst #{const GL_FRONT_AND_BACK} = FrontAndBack
    fromConst #{const GL_AUX0} = Aux0
    fromConst #{const GL_AUX1} = Aux1
    --fromConst #{const GL_FRAMEBUFFER_EXT} = FramebufferEXT
    --fromConst #{const GL_FRAMEBUFFER_COMPLETE_EXT} = FramebufferCompleteEXT
    -- internal format
    fromConst #{const GL_RGB} = RGB
    -- blend / depth
    fromConst #{const GL_ZERO}                = Zero
    fromConst #{const GL_ONE}                 = One
    fromConst #{const GL_DST_COLOR}           = DestColor
    fromConst #{const GL_ONE_MINUS_DST_COLOR} = OneMinusDestColor
    fromConst #{const GL_SRC_ALPHA}           = SourceAlpha
    fromConst #{const GL_ONE_MINUS_SRC_ALPHA} = OneMinusSourceAlpha
    fromConst #{const GL_DST_ALPHA}           = DestAlpha
    fromConst #{const GL_ONE_MINUS_DST_ALPHA} = OneMinusDestAlpha
    -- texture params
    fromConst #{const GL_NEAREST} = Nearest

-------

a & b = a (toConst b)

-- # define CC foreign import ccall unsafe

--foreign import ccall unsafe "glFlush" flush :: IO ()
foreign import CALLCONV unsafe "glBegin"        begin        :: GLenum     -> IO ()
foreign import CALLCONV unsafe "glEnd"          end          :: IO ()
foreign import CALLCONV unsafe "glLoadIdentity" loadIdentity :: IO ()
foreign import CALLCONV unsafe "glPushMatrix"   pushMatrix   :: IO ()
foreign import CALLCONV unsafe "glPopMatrix"    popMatrix    :: IO ()
foreign import CALLCONV unsafe "glPushAttrib"   pushAttrib   :: GLbitfield -> IO ()
foreign import CALLCONV unsafe "glPopAttrib"    popAttrib    :: IO ()
foreign import CALLCONV unsafe "glMatrixMode"   matrixMode   :: GLenum     -> IO ()
foreign import CALLCONV unsafe "glEnable"       enable       :: GLenum     -> IO ()
foreign import CALLCONV unsafe "glDisable"      disable      :: GLenum     -> IO ()
foreign import CALLCONV unsafe "glViewport"     viewport     :: GLint      -> GLint    -> GLsizei  -> GLsizei -> IO ()
foreign import CALLCONV unsafe "glFrustum"      frustum      :: GLdouble   -> GLdouble -> GLdouble -> GLdouble
                                                             -> GLdouble   -> GLdouble -> IO ()
foreign import CALLCONV unsafe "glOrtho"        ortho        :: GLdouble   -> GLdouble -> GLdouble -> GLdouble
                                                             -> GLdouble   -> GLdouble -> IO ()
foreign import CALLCONV unsafe "glScissor"      scissor      :: GLint      -> GLint    -> GLsizei  -> GLsizei -> IO ()

foreign import CALLCONV unsafe "glClear"      clear      :: GLbitfield -> IO ()
foreign import CALLCONV unsafe "glClearColor" clearColor :: GLclampf   -> GLclampf  -> GLclampf -> GLclampf -> IO ()
foreign import CALLCONV unsafe "glColorMask"  colorMask  :: GLboolean  -> GLboolean -> GLboolean -> GLboolean -> IO ()
foreign import CALLCONV unsafe "glDepthMask"  depthMask  :: GLboolean  -> IO ()
foreign import CALLCONV unsafe "glClearDepth" clearDepth :: GLclampd   -> IO ()
foreign import CALLCONV unsafe "glDepthFunc"  depthFunc  :: GLenum     -> IO ()
foreign import CALLCONV unsafe "glBlendFunc"  blendFunc  :: GLenum     -> GLenum    -> IO ()

foreign import CALLCONV unsafe "glGenTextures"    genTextures    :: GLsizei -> Ptr GLuint -> IO ()
foreign import CALLCONV unsafe "glBindTexture"    bindTexture    :: GLenum  -> GLuint     -> IO ()
foreign import CALLCONV unsafe "glDeleteTextures" deleteTextures :: GLsizei -> Ptr GLuint -> IO ()

-- second arg for this is actually GLint
--foreign import ccall unsafe "gluBuild2DMipmaps" build2DMipmaps :: GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()
-- third arg for this is actually GLint
foreign import CALLCONV unsafe "glTexImage2D"    texImage2D    :: GLenum  -> GLint  -> GLenum -> GLsizei
                                                               -> GLsizei -> GLint  -> GLenum -> GLenum -> Ptr a -> IO ()
foreign import CALLCONV unsafe "glTexParameteri" texParameteri :: GLenum  -> GLenum -> GLenum -> IO ()

foreign import CALLCONV unsafe "glShadeModel"    shadeModel    :: GLenum  -> IO ()
foreign import CALLCONV unsafe "glFrontFace"     frontFace     :: GLenum  -> IO ()
foreign import CALLCONV unsafe "glCullFace"      cullFace      :: GLenum  -> IO ()
foreign import CALLCONV unsafe "glPolygonMode"   polygonMode   :: GLenum  -> GLenum  -> IO ()
foreign import CALLCONV unsafe "glPolygonOffset" polygonOffset :: GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glPointSize"     pointSize     :: GLfloat -> IO ()
foreign import CALLCONV unsafe "glLineWidth"     lineWidth     :: GLfloat -> IO ()

-- float or double funs

foreign import CALLCONV unsafe "glRotatef"    rotatef    :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glScalef"     scalef     :: GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glTranslatef" translatef :: GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glColor3f"    color3f    :: GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glColor4f"    color4f    :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glVertex2f"   vertex2f   :: GLfloat -> GLfloat -> IO ()
--foreign import CALLCONV unsafe "glVertex2fv" vertex2fv :: Ptr GLfloat -> IO ()
foreign import CALLCONV unsafe "glVertex3f"   vertex3f   :: GLfloat -> GLfloat -> GLfloat -> IO ()
--foreign import CALLCONV unsafe "glVertex3fv" vertex3fv :: Ptr GLfloat -> IO ()
foreign import CALLCONV unsafe "glTexCoord2f" texCoord2f :: GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glRotated"    rotated    :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
foreign import CALLCONV unsafe "glScaled"     scaled     :: GLdouble -> GLdouble -> GLdouble -> IO ()
foreign import CALLCONV unsafe "glTranslated" translated :: GLdouble -> GLdouble -> GLdouble -> IO ()
foreign import CALLCONV unsafe "glColor3d"    color3d    :: GLdouble -> GLdouble -> GLdouble -> IO ()
foreign import CALLCONV unsafe "glColor4d"    color4d    :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
foreign import CALLCONV unsafe "glVertex2d"   vertex2d   :: GLdouble -> GLdouble -> IO ()
--foreign import CALLCONV unsafe "glVertex2dv" vertex2dv :: Ptr GLdouble -> IO ()
foreign import CALLCONV unsafe "glVertex3d"   vertex3d   :: GLdouble -> GLdouble -> GLdouble -> IO ()
--foreign import CALLCONV unsafe "glVertex3dv" 
foreign import CALLCONV unsafe "glTexCoord2d" texCoord2d :: GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glRectf" rectf :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import CALLCONV unsafe "glRecti" recti :: GLint   -> GLint   -> GLint   -> GLint   -> IO ()

{-
foreign import ccall unsafe "glGenBuffers" genBuffers :: GLsizei -> Ptr GLuint -> IO ()
foreign import ccall unsafe "glBindBuffer" bindBuffer :: GLenum -> GLuint -> IO ()
foreign import ccall unsafe "glBufferData" bufferData :: GLenum -> GLsizeiptr -> Ptr a -> GLenum -> IO ()
foreign import ccall unsafe "glMapBuffer" mapBuffer :: GLenum -> GLenum -> IO (Ptr a)
foreign import ccall unsafe "glUnmapBuffer" unmapBuffer :: GLenum -> IO GLboolean
-}

foreign import CALLCONV unsafe "glDrawBuffer" drawBuffer :: GLenum -> IO ()
foreign import CALLCONV unsafe "glReadBuffer" readBuffer :: GLenum -> IO ()

foreign import CALLCONV unsafe "glReadPixels"        readPixels        :: GLint  -> GLint   -> GLsizei -> GLsizei
                                                                       -> GLenum -> GLenum  -> Ptr a   -> IO ()
foreign import CALLCONV unsafe "glCopyPixels"        copyPixels        :: GLint  -> GLint   -> GLsizei -> GLsizei
                                                                       -> GLenum -> IO ()
foreign import CALLCONV unsafe "glCopyTexImage2D"    copyTexImage2D    :: GLenum -> GLint   -> GLenum  -> GLint
                                                                       -> GLint  -> GLsizei -> GLsizei -> GLint   -> IO ()
foreign import CALLCONV unsafe "glCopyTexSubImage2D" copyTexSubImage2D :: GLenum -> GLint   -> GLint   -> GLint
                                                                       -> GLint  -> GLint   -> GLsizei -> GLsizei -> IO ()

{-
foreign import ccall unsafe "glGenFramebuffersEXT" genFramebuffersEXT :: GLsizei -> Ptr GLuint -> IO ()
foreign import ccall unsafe "glGenRenderbuffersEXT" genRenderbuffersEXT :: GLsizei -> Ptr GLuint -> IO ()
foreign import ccall unsafe "glBindFramebufferEXT" bindFramebufferEXT :: GLenum -> GLuint -> IO ()
foreign import ccall unsafe "glBindRenderbufferEXT" bindRenderbufferEXT :: GLenum -> GLuint -> IO ()
foreign import ccall unsafe "glFramebufferTexture2DEXT" framebufferTexture2DEXT :: GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ()
foreign import ccall unsafe "glRenderbufferStorageEXT" renderbufferStorageEXT :: GLenum -> GLenum -> GLsizei -> GLsizei -> IO ()
foreign import ccall unsafe "glCheckFramebufferStatusEXT" checkFramebufferStatusEXT :: GLenum -> IO GLenum
foreign import ccall unsafe "glDeleteFramebuffersEXT" deleteFramebuffersEXT :: GLsizei -> Ptr GLuint -> IO ()
foreign import ccall unsafe "glGenerateMipmapEXT" generateMipmapsEXT :: GLenum -> IO ()
foreign import ccall unsafe "glDeleteRenderbuffersEXT" deleteRenderbuffersEXT :: GLsizei -> Ptr GLuint -> IO ()
foreign import ccall unsafe "glFramebufferRenderbufferEXT" framebufferRenderbufferEXT :: GLenum -> GLenum -> GLenum -> GLuint -> IO ()
-}

-- also used in AL
--glGen :: (GLsizei -> Ptr GLuint -> IO ()) -> IO GLuint
glGen f = alloca (\ptr -> f 1 ptr >> peek ptr)
