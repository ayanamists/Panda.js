"use client";

import { Listbox, ListboxItem } from '@nextui-org/listbox';
import PostCard from '@/components/PostCard';
import { Post } from '@/contents/cms';
import { formatDate } from '@/utils';
import { Link } from '@/navigation';

function PostDate({ date }: { date: string }) {
  return (
    <time className="text-sm font-semibold text-gray-500 font-mono">
      {formatDate(date)}
    </time>
  );
}

export default function PostList({ posts }: { posts: Post[] }) {
  return (
    <Listbox aria-label='Post List' className='mt-6'>
      {posts.map(post => (
        <ListboxItem
          key={post.id}
          href={`/posts/${post.id}`}
          as={Link}
        >
          <div className='sm:flex items-center'>
            <div className='w-24'>
              <PostDate date={post.metaData.date} />
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

