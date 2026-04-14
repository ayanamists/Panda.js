export default function Loading() {
  return (
    <div role="status" className="flex justify-center items-center h-[60vh]">
      <div className="flex gap-1.5">
        <span className="w-2 h-2 rounded-full bg-primary/40 animate-bounce [animation-delay:0ms]" />
        <span className="w-2 h-2 rounded-full bg-primary/40 animate-bounce [animation-delay:150ms]" />
        <span className="w-2 h-2 rounded-full bg-primary/40 animate-bounce [animation-delay:300ms]" />
      </div>
      <span className="sr-only">Loading...</span>
    </div>);
}
