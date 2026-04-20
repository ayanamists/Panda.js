export default function Container({ children }: { children: React.ReactNode }) {
  return (
    <div className="
    relative w-full py-16
    lg:mx-auto lg:max-w-4xl lg:pt-20 lg:pb-32
    bg-lightwhite dark:bg-lightblack">
      <div className="prose prose-natural
      dark:prose-invert mx-auto max-w-full px-8 sm:px-16
      relative">
        {children}
      </div>
    </div>
  )
}
