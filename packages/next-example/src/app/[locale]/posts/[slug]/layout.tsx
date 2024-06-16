export default function Container({ children }: { children: React.ReactNode }) {
  return (
    <div className="
    relative w-full py-12 shadow-xl
    shadow-slate-700/20 ring-1 ring-gray-900/5
    dark:shadow-slate-100/20 dark:ring-gray-100/5
    lg:mx-auto lg:max-w-4xl lg:pt-16 lg:pb-28">
      <div className="prose prose-natural
      dark:prose-invert mx-auto max-w-full px-8 sm:px-16
      relative">
        {children}
      </div>
    </div>
  )
}
