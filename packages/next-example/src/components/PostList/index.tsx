"use client";

import { Listbox, ListboxItem } from '@nextui-org/listbox';
import PostCard from '@/components/PostCard';
import { Post } from '@/contents/cms';

export default function PostList({ posts }: { posts: Post[] }) {
  return (
    <Listbox aria-label='Post List' className='mt-6'>
      {posts.map(post => (
        <ListboxItem key={post.id}
          textValue={post.metaData.title}
        >
          <div className='sm:flex items-center'>
            <div className='w-24'>
              <time className="text-sm font-semibold text-gray-500 font-mono">
                {formatDate(new Date(post.metaData.date))}
              </time>
            </div>
            <div>
              <PostCard post={post} />
            </div>
          </div>
        </ListboxItem>
      ))}
    </Listbox>
  );
}

function formatDate(d: Date) {
  return formatNum(d.getDate()) + "/" +
    formatNum(d.getMonth() + 1) + "/" + d.getFullYear();
}

function formatNum(n: number) {
  return n.toLocaleString('en-US', { minimumIntegerDigits: 2, useGrouping: false })
}
