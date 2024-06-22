"use client";

import './index.css';
import { useEffect, useRef, useState, useCallback } from 'react';


interface TableOfContentsProps {
  children: React.ReactNode
}

export default function TableOfContents({ children }: TableOfContentsProps) {

  // titles, the document itself, should not change, or some bugs can happen
  const titles= useRef<Element[] | null>(null);

  const [active, setActive] = useState<Element | null>(null);
  const preActiveRef = useRef<Element | null>(null);

  // deep dark method for sync toc with user view
  const onScroll = useCallback(() => {
    const elements = titles.current;
    if (elements == null) {
      return;
    }
    const viewHeight = window.innerHeight;
    const disToMiddle = elements.map((e) => {
      const rect = e.getBoundingClientRect();
      const dis = rect.y - viewHeight / 4;
      return {dis, e};
    });

    disToMiddle.sort((a, b) => a.dis - b.dis);
    const negs = disToMiddle.filter((x) => x.dis <= 0);
    const last = negs.pop();
    if (last == null) {
      setActive(null);
    } else {
      const id = last.e.id;

      // FIXME: update react component with dom api can cause problem,
      // any clean way?
      // NOTE: hard encoding id name for toc, it's autogen by pandoc,
      // if change the pandoc backend, fix this.
      const activeTOCElem = document.querySelector(`#toc-${id}`);
      if (activeTOCElem != null) {
        setActive(activeTOCElem);
      }
    }
  }, []);

  useEffect(() => {
    titles.current = Array.from(document.querySelectorAll('h2, h3, h4, h5, h6'));
    window.addEventListener('scroll', onScroll);
    // call onScroll once for correctly highlight the element for first render
    onScroll();
    return () => {
      window.removeEventListener('scroll', onScroll);
      titles.current = null;
    }
  }, [onScroll, titles]);

  const activeClass = `active`;
  const cleanPreActive = useCallback(() => {
    const preActive = preActiveRef.current;
    if (preActive != null) {
      preActive.classList.remove(activeClass);
    }
  }, [preActiveRef, activeClass]);
  useEffect(() => {
    active?.classList.add(activeClass);
    preActiveRef.current = active;
    return cleanPreActive;
  }, [active, cleanPreActive, activeClass]);

  // NOTE: this will only work when outer block is not static block
  // so I set the blog body to relative
  return (<div className="xl:absolute xl:-right-8">
    <div className="xl:fixed xl:max-w-[300px]">
      <div className="toc">
      {children}
      </div>
    </div>
  </div>);
}
