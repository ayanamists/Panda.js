/**
 * 集字 calligraphy rendering of 李晨曦
 *
 * Option A — all 二王 lineage kaishu:
 *   李 — 褚遂良   (Tang, 二王 lineage)
 *   晨 — 褚遂良   (Tang, 二王 lineage)
 *   曦 — 高正臣   (Tang, 二王 style)
 *
 * Option B — mixed 二王:
 *   李 — 王羲之   (Eastern Jin, 二王 origin)
 *   晨 — 褚遂良   (Tang, 二王 lineage)
 *   曦 — 高正臣   (Tang, 二王 style)
 *
 * Toggle: change `variant` below.
 */

import optALi from './opt_a_li';
import optAChen from './opt_a_chen';
import optAXi from './opt_a_xi';
import optBLi from './opt_b_li';

// ← Change to 'B' to preview Option B
const variant: 'A' | 'B' = 'B';

const sets = {
  A: { li: optALi, chen: optAChen, xi: optAXi },
  B: { li: optBLi, chen: optAChen, xi: optAXi },
};

function Glyph({ d, viewBox, label, className }: { d: string; viewBox: string; label: string; className?: string }) {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox={viewBox}
      className={`h-[1em] w-auto ${className ?? ''}`}
      aria-hidden="true"
      role="img"
    >
      <title>{label}</title>
      <path d={d} fill="currentColor" fillRule="evenodd" />
    </svg>
  );
}

export default function CalligraphyName({ className }: { className?: string }) {
  const s = sets[variant];
  return (
    <span className={`inline-flex items-end gap-0.5 ${className ?? ''}`} aria-label="李晨曦">
      <Glyph d={s.li} viewBox="0 0 370 408" label="李" />
      <Glyph d={s.chen} viewBox="0 0 370 408" label="晨" />
      <Glyph d={s.xi} viewBox="0 0 370 370" label="曦" className="-translate-y-[0.06em] scale-105" />
    </span>
  );
}
