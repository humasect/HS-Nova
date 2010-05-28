{-# OPTIONS -XForeignFunctionInterface -cpp -XMultiParamTypeClasses -XTypeSynonymInstances #-}
module Nova.AL where

#ifdef __APPLE__
#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#else
#include <AL/al.h>
#include <AL/alc.h>
#endif

#ifndef CALLCONV
#define CALLCONV ccall
#endif

import Foreign.Ptr
import Data.Word
import Data.Int

import Nova.GL (ToConst(..),FromConst(..))

type ALenum  = #{type ALenum}
type ALfloat = #{type ALfloat}
type ALuint  = #{type ALuint}
type ALsizei = #{type ALsizei}
type ALbyte  = #{type ALbyte}
type ALint   = #{type ALint}

data ALConstants =
      Position | Velocity | Orientation | SourceRelative | Direction
    | Pitch | Gain | Buffer | Looping | RolloffFactor
    | FormatMono8 | FormatMono16 | FormatStereo8 | FormatStereo16
    | NoError
    | Playing | BuffersProcessed | BuffersQueued | SourceState
  deriving (Eq)

instance ToConst ALConstants ALenum where
    toConst Position         = #{const AL_POSITION}
    toConst Velocity         = #{const AL_VELOCITY}
    toConst Orientation      = #{const AL_ORIENTATION}
    toConst Direction        = #{const AL_DIRECTION}
    toConst Pitch            = #{const AL_PITCH}
    toConst Gain             = #{const AL_GAIN}
    toConst Buffer           = #{const AL_BUFFER}
    toConst Looping          = #{const AL_LOOPING}
    toConst FormatMono8      = #{const AL_FORMAT_MONO8}
    toConst FormatMono16     = #{const AL_FORMAT_MONO16}
    toConst FormatStereo8    = #{const AL_FORMAT_STEREO8}
    toConst FormatStereo16   = #{const AL_FORMAT_STEREO16}
    toConst RolloffFactor    = #{const AL_ROLLOFF_FACTOR}
    toConst Playing          = #{const AL_PLAYING}
    toConst BuffersProcessed = #{const AL_BUFFERS_PROCESSED}
    toConst BuffersQueued    = #{const AL_BUFFERS_QUEUED}
    toConst SourceRelative   = #{const AL_SOURCE_RELATIVE}
    toConst SourceState      = #{const AL_SOURCE_STATE}
    toConst NoError          = #{const AL_NO_ERROR}

instance FromConst ALConstants ALenum where
    fromConst #{const AL_POSITION}        = Position
    fromConst #{const AL_VELOCITY}        = Velocity
    fromConst #{const AL_ORIENTATION}     = Orientation
    fromConst #{const AL_PITCH}           = Pitch
    fromConst #{const AL_GAIN}            = Gain
    fromConst #{const AL_BUFFER}          = Buffer
    fromConst #{const AL_LOOPING}         = Looping
    fromConst #{const AL_FORMAT_MONO8}    = FormatMono8
    fromConst #{const AL_FORMAT_MONO16}   = FormatMono16
    fromConst #{const AL_FORMAT_STEREO8}  = FormatStereo8
    fromConst #{const AL_FORMAT_STEREO16} = FormatStereo16
    fromConst #{const AL_PLAYING}         = Playing
    fromConst #{const AL_NO_ERROR}        = NoError
    fromConst #{const AL_SOURCE_STATE}    = SourceState

foreign import CALLCONV unsafe "alGetError" getError :: IO ALenum

foreign import CALLCONV unsafe "alListenerf"  listenerf  :: ALenum -> ALfloat -> IO ()
foreign import CALLCONV unsafe "alListener3f" listener3f :: ALenum -> ALfloat -> ALfloat -> ALfloat -> IO ()
foreign import CALLCONV unsafe "alListeneri"  listeneri  :: ALenum -> ALint   -> IO ()
foreign import CALLCONV unsafe "alListener3i" listener3i :: ALenum -> ALint   -> ALint   -> ALint   -> IO ()

foreign import CALLCONV unsafe "alSourcef"  sourcef  :: ALuint -> ALenum -> ALfloat -> IO ()
foreign import CALLCONV unsafe "alSource3f" source3f :: ALuint -> ALenum -> ALfloat -> ALfloat -> ALfloat -> IO ()
foreign import CALLCONV unsafe "alSourcei"  sourcei  :: ALuint -> ALenum -> ALint   -> IO ()
foreign import CALLCONV unsafe "alSource3i" source3i :: ALuint -> ALenum -> ALint   -> ALint   -> ALint   -> IO ()

foreign import CALLCONV unsafe "alGetSourcei"    getSourcei    :: ALuint  ->     ALenum -> Ptr ALint -> IO ()
foreign import CALLCONV unsafe "alGenSources"    genSources    :: ALuint  -> Ptr ALuint -> IO ()
foreign import CALLCONV unsafe "alDeleteSources" deleteSources :: ALsizei -> Ptr ALuint -> IO ()

foreign import CALLCONV unsafe "alSourceQueueBuffers"   sourceQueueBuffers   :: ALuint -> ALsizei -> Ptr ALuint -> IO ()
foreign import CALLCONV unsafe "alSourceUnqueueBuffers" sourceUnqueueBuffers :: ALuint -> ALsizei -> Ptr ALuint -> IO ()

foreign import CALLCONV unsafe "alGenBuffers"    genBuffers    :: ALsizei -> Ptr ALuint -> IO ()
foreign import CALLCONV unsafe "alBufferData"    bufferData    :: ALuint  ->     ALenum -> Ptr a -> ALsizei
                                                               -> ALsizei -> IO ()
foreign import CALLCONV unsafe "alDeleteBuffers" deleteBuffers :: ALsizei -> Ptr ALuint -> IO ()

foreign import CALLCONV unsafe "alSourcePlay"   sourcePlay   :: ALuint -> IO ()
foreign import CALLCONV unsafe "alSourceStop"   sourceStop   :: ALuint -> IO ()
foreign import CALLCONV unsafe "alSourceRewind" sourceRewind :: ALuint -> IO ()
foreign import CALLCONV unsafe "alSourcePause"  sourcePause  :: ALuint -> IO ()

