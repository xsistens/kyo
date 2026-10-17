// Hand-maintained residue of the retired CemGen (enum shapes originally derived
// from @ui5/webcomponents 2.24.0 type declarations; now owned by hand).
// (Trimmed during the PrimeOne migration: only the enums still used by live
// components remain — retired ones left with their components.)
package kyo.uic

/** TextEmptyIndicatorMode (from @ui5/webcomponents dist/types/TextEmptyIndicatorMode.d.ts). */
enum TextEmptyIndicatorMode derives CanEqual:
    case Off, On

    private[uic] def token: String = this.toString
end TextEmptyIndicatorMode

/** TitleLevel (from @ui5/webcomponents dist/types/TitleLevel.d.ts). */
enum TitleLevel derives CanEqual:
    case H1, H2, H3, H4, H5, H6

    private[uic] def token: String = this.toString
end TitleLevel
