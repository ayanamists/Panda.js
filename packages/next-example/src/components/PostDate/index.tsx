const MONTHS = [
  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
];

function PostDate({ date, compact }: { date: string; compact?: boolean }) {
  const d = new Date(date);
  const day = d.getDate();
  const month = MONTHS[d.getMonth()];
  const year = d.getFullYear();

  const formatted = compact
    ? `${month} ${day}`
    : `${day} ${month} ${year}`;

  return (
    <time
      dateTime={d.toISOString().slice(0, 10)}
      className="text-[13px] text-foreground/35 font-heading tracking-wide tabular-nums"
    >
      {formatted}
    </time>
  );
}

export default PostDate;
