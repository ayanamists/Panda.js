import { formatDate } from "@/utils";

function PostDate({ date }: { date: string }) {
  return (
    <time className="text-sm text-foreground/40 font-mono tracking-wide">
      {formatDate(date)}
    </time>
  );
}

export default PostDate;
