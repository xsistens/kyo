package kyo.scheduler

import kyo.StaticFlag

private[kyo] object coreWorkers    extends StaticFlag[Int](Runtime.getRuntime().availableProcessors(), n => Right(Math.max(1, n)))
private[kyo] object minWorkers     extends StaticFlag[Int]((coreWorkers().toDouble / 2).intValue(), n => Right(Math.max(1, n)))
private[kyo] object maxWorkers     extends StaticFlag[Int](coreWorkers() * 100, n => Right(Math.max(minWorkers(), n)))
private[kyo] object scheduleStride extends StaticFlag[Int](Runtime.getRuntime().availableProcessors(), n => Right(Math.max(1, n)))
private[kyo] object stealStride    extends StaticFlag[Int](Runtime.getRuntime().availableProcessors() * 8, n => Right(Math.max(1, n)))

private[kyo] object virtualizeWorkers extends StaticFlag[Boolean](false)
private[kyo] object timeSliceMs       extends StaticFlag[Int](10)
// JS only: wall-clock budget for one drain of the scheduler's run queue. Clamped to timeSliceMs so a
// drained turn can never block longer than the single-task form it replaced.
private[kyo] object drainBudgetMs      extends StaticFlag[Int](4, n => Right(Math.max(1, Math.min(timeSliceMs(), n))))
private[kyo] object cycleIntervalNs    extends StaticFlag[Int](100000)
private[kyo] object enableTopJMX       extends StaticFlag[Boolean](false)
private[kyo] object enableTopConsoleMs extends StaticFlag[Int](0)
private[kyo] object topStatusFile      extends StaticFlag[String]("")
private[kyo] object topStatusFileMs    extends StaticFlag[Int](0)

// worker rebalance rate-limit (ms)
private[kyo] object minInterval extends StaticFlag[Int](1)
// WorkerQueue initial backing-array size
private[kyo] object queueCapacity extends StaticFlag[Int](8, n => Right(Math.max(1, n)))
