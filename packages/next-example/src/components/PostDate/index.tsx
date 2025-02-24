import { formatDate } from "@/utils";

function PostDate({ date }: { date: string }) {
  return (
    <time className="text-sm font-semibold text-gray-500 font-mono">
      {formatDate(date)}
    </time>
  );
}

export default PostDate;
