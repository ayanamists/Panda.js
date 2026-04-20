"use client";

import './index.css';
import { useEffect, useRef, useState, useCallback } from 'react';

interface TableOfContentsProps {
  children: React.ReactNode
}

function getActiveHeadingId(headings: Element[]): string | null {
  const threshold = window.innerHeight / 4;
  let activeId: string | null = null;

  for (const heading of headings) {
    if (heading.getBoundingClientRect().top > threshold) break;
    activeId = heading.id;
  }

  return activeId;
}

export default function TableOfContents({ children }: TableOfContentsProps) {
  const [activeId, setActiveId] = useState<string | null>(null);
  const rafRef = useRef(0);

  const onScroll = useCallback(() => {
    cancelAnimationFrame(rafRef.current);
    rafRef.current = requestAnimationFrame(() => {
      const headings = Array.from(document.querySelectorAll('h2, h3, h4, h5, h6'));
      setActiveId(getActiveHeadingId(headings));
    });
  }, []);

  useEffect(() => {
    onScroll();
    window.addEventListener('scroll', onScroll, { passive: true });
    return () => {
      window.removeEventListener('scroll', onScroll);
      cancelAnimationFrame(rafRef.current);
    };
  }, [onScroll]);

  // Sync active class on TOC link via DOM — children are server-rendered,
  // so we can't pass props to individual links.
  useEffect(() => {
    if (!activeId) return;
    // TOC link IDs are prefixed with "toc-" by the Pandoc backend
    const el = document.getElementById(`toc-${activeId}`);
    if (!el) return;
    el.classList.add('active');
    return () => el.classList.remove('active');
  }, [activeId]);

  return (
    <div className="xl:absolute xl:-right-8 mt-2">
      <div className="xl:fixed xl:max-w-xs">
        <div className="toc font-heading text-[12px]">
          {children}
        </div>
      </div>
    </div>
  );
}
