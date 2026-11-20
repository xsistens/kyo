package kyo

import kyo.Signal.SignalRef

private[kyo] object SignalPlatformSpecific:

    inline def hubSlot = Maybe.empty[SignalRef.Unsafe.HubSlot]

end SignalPlatformSpecific
