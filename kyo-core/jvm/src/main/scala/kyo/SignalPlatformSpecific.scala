package kyo

import java.lang.invoke.MethodHandles
import kyo.Signal.SignalRef

private[kyo] object SignalPlatformSpecific:

    val hubSlot: Maybe[SignalRef.Unsafe.HubSlot] =
        val owner     = classOf[SignalRef.Root[?]]
        val varHandle = MethodHandles.privateLookupIn(owner, MethodHandles.lookup()).unreflectVarHandle(owner.getDeclaredField("hub"))
        Present(
            new SignalRef.Unsafe.HubSlot:
                def install(ref: SignalRef.Root[?], hub: AnyRef): Boolean =
                    varHandle.compareAndSet(ref, null: AnyRef, hub)
        )
    end hubSlot

end SignalPlatformSpecific
