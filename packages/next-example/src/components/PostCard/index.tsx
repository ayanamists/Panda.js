"use client";

import { Post } from '@/contents/cms';

export default function PostCard({ post }: { post: Post }) {
  return (
    <span
      className='text-base whitespace-normal text-foreground/85 hover:text-primary transition-colors duration-200 font-mainpage'
      aria-label='Post'
    >
      {post.metaData.title}
    </span>
  );
}
