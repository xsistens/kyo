package kyo

import kyo.Signal.SignalRef
import scala.scalanative.libc.stdatomic.AtomicRef
import scala.scalanative.runtime.Intrinsics
import scala.scalanative.runtime.fromRawPtr

private[kyo] object SignalPlatformSpecific:

    val hubSlot: Maybe[SignalRef.Unsafe.HubSlot] =
        Present(
            new SignalRef.Unsafe.HubSlot:
                def install(ref: SignalRef.Root[?], hub: AnyRef): Boolean =
                    val field = AtomicRef[AnyRef](fromRawPtr(Intrinsics.classFieldRawPtr(ref, "hub")))
                    field.compareExchangeStrong(null: AnyRef, hub)
        )

end SignalPlatformSpecific
