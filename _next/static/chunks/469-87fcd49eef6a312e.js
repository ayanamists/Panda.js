"use strict";(self.webpackChunk_N_E=self.webpackChunk_N_E||[]).push([[469],{9912:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"addLocale",{enumerable:!0,get:function(){return r}});n(2679);const r=function(e){for(var t=arguments.length,n=new Array(t>1?t-1:0),r=1;r<t;r++)n[r-1]=arguments[r];return e};("function"===typeof t.default||"object"===typeof t.default&&null!==t.default)&&"undefined"===typeof t.default.__esModule&&(Object.defineProperty(t.default,"__esModule",{value:!0}),Object.assign(t.default,t),e.exports=t.default)},2774:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"getDomainLocale",{enumerable:!0,get:function(){return r}});n(2679);function r(e,t,n,r){return!1}("function"===typeof t.default||"object"===typeof t.default&&null!==t.default)&&"undefined"===typeof t.default.__esModule&&(Object.defineProperty(t.default,"__esModule",{value:!0}),Object.assign(t.default,t),e.exports=t.default)},5469:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"default",{enumerable:!0,get:function(){return R}});const r=n(7666),o=n(7573),a=r._(n(9585)),i=n(5913),u=n(5499),s=n(3570),l=n(6e3),c=n(9912),f=n(1917),d=n(5026),p=n(3868),h=n(2774),m=n(5923),g=n(5398),y=new Set;function b(e,t,n,r,o,a){if("undefined"===typeof window)return;if(!a&&!(0,u.isLocalURL)(t))return;if(!r.bypassPrefetchedCheck){const o=t+"%"+n+"%"+("undefined"!==typeof r.locale?r.locale:"locale"in e?e.locale:void 0);if(y.has(o))return;y.add(o)}const i=a?e.prefetch(t,o):e.prefetch(t,n,r);Promise.resolve(i).catch((e=>{0}))}function P(e){return"string"===typeof e?e:(0,s.formatUrl)(e)}const R=a.default.forwardRef((function(e,t){let n;const{href:r,as:s,children:y,prefetch:R=null,passHref:_,replace:v,shallow:O,scroll:j,locale:w,onClick:E,onMouseEnter:S,onTouchStart:x,legacyBehavior:M=!1,...N}=e;n=y,!M||"string"!==typeof n&&"number"!==typeof n||(n=(0,o.jsx)("a",{children:n}));const k=a.default.useContext(f.RouterContext),C=a.default.useContext(d.AppRouterContext),I=null!=k?k:C,T=!k,L=!1!==R,U=null===R?g.PrefetchKind.AUTO:g.PrefetchKind.FULL;const{href:A,as:W}=a.default.useMemo((()=>{if(!k){const e=P(r);return{href:e,as:s?P(s):e}}const[e,t]=(0,i.resolveHref)(k,r,!0);return{href:e,as:s?(0,i.resolveHref)(k,s):t||e}}),[k,r,s]),K=a.default.useRef(A),z=a.default.useRef(W);let D;M&&(D=a.default.Children.only(n));const q=M?D&&"object"===typeof D&&D.ref:t,[B,F,$]=(0,p.useIntersection)({rootMargin:"200px"}),Y=a.default.useCallback((e=>{z.current===W&&K.current===A||($(),z.current=W,K.current=A),B(e),q&&("function"===typeof q?q(e):"object"===typeof q&&(q.current=e))}),[W,q,A,$,B]);a.default.useEffect((()=>{I&&F&&L&&b(I,A,W,{locale:w},{kind:U},T)}),[W,A,F,w,L,null==k?void 0:k.locale,I,T,U]);const H={ref:Y,onClick(e){M||"function"!==typeof E||E(e),M&&D.props&&"function"===typeof D.props.onClick&&D.props.onClick(e),I&&(e.defaultPrevented||function(e,t,n,r,o,i,s,l,c){const{nodeName:f}=e.currentTarget;if("A"===f.toUpperCase()&&(function(e){const t=e.currentTarget.getAttribute("target");return t&&"_self"!==t||e.metaKey||e.ctrlKey||e.shiftKey||e.altKey||e.nativeEvent&&2===e.nativeEvent.which}(e)||!c&&!(0,u.isLocalURL)(n)))return;e.preventDefault();const d=()=>{const e=null==s||s;"beforePopState"in t?t[o?"replace":"push"](n,r,{shallow:i,locale:l,scroll:e}):t[o?"replace":"push"](r||n,{scroll:e})};c?a.default.startTransition(d):d()}(e,I,A,W,v,O,j,w,T))},onMouseEnter(e){M||"function"!==typeof S||S(e),M&&D.props&&"function"===typeof D.props.onMouseEnter&&D.props.onMouseEnter(e),I&&(!L&&T||b(I,A,W,{locale:w,priority:!0,bypassPrefetchedCheck:!0},{kind:U},T))},onTouchStart:function(e){M||"function"!==typeof x||x(e),M&&D.props&&"function"===typeof D.props.onTouchStart&&D.props.onTouchStart(e),I&&(!L&&T||b(I,A,W,{locale:w,priority:!0,bypassPrefetchedCheck:!0},{kind:U},T))}};if((0,l.isAbsoluteUrl)(W))H.href=W;else if(!M||_||"a"===D.type&&!("href"in D.props)){const e="undefined"!==typeof w?w:null==k?void 0:k.locale,t=(null==k?void 0:k.isLocaleDomain)&&(0,h.getDomainLocale)(W,e,null==k?void 0:k.locales,null==k?void 0:k.domainLocales);H.href=t||(0,m.addBasePath)((0,c.addLocale)(W,e,null==k?void 0:k.defaultLocale))}return M?a.default.cloneElement(D,H):(0,o.jsx)("a",{...N,...H,children:n})}));("function"===typeof t.default||"object"===typeof t.default&&null!==t.default)&&"undefined"===typeof t.default.__esModule&&(Object.defineProperty(t.default,"__esModule",{value:!0}),Object.assign(t.default,t),e.exports=t.default)},6790:function(e,t){Object.defineProperty(t,"__esModule",{value:!0}),function(e,t){for(var n in t)Object.defineProperty(e,n,{enumerable:!0,get:t[n]})}(t,{cancelIdleCallback:function(){return r},requestIdleCallback:function(){return n}});const n="undefined"!==typeof self&&self.requestIdleCallback&&self.requestIdleCallback.bind(window)||function(e){let t=Date.now();return self.setTimeout((function(){e({didTimeout:!1,timeRemaining:function(){return Math.max(0,50-(Date.now()-t))}})}),1)},r="undefined"!==typeof self&&self.cancelIdleCallback&&self.cancelIdleCallback.bind(window)||function(e){return clearTimeout(e)};("function"===typeof t.default||"object"===typeof t.default&&null!==t.default)&&"undefined"===typeof t.default.__esModule&&(Object.defineProperty(t.default,"__esModule",{value:!0}),Object.assign(t.default,t),e.exports=t.default)},5913:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"resolveHref",{enumerable:!0,get:function(){return f}});const r=n(6537),o=n(3570),a=n(9084),i=n(6e3),u=n(2679),s=n(5499),l=n(7145),c=n(3929);function f(e,t,n){let f,d="string"===typeof t?t:(0,o.formatWithValidation)(t);const p=d.match(/^[a-zA-Z]{1,}:\/\//),h=p?d.slice(p[0].length):d;if((h.split("?",1)[0]||"").match(/(\/\/|\\)/)){console.error("Invalid href '"+d+"' passed to next/router in page: '"+e.pathname+"'. Repeated forward-slashes (//) or backslashes \\ are not valid in the href.");const t=(0,i.normalizeRepeatedSlashes)(h);d=(p?p[0]:"")+t}if(!(0,s.isLocalURL)(d))return n?[d]:d;try{f=new URL(d.startsWith("#")?e.asPath:e.pathname,"http://n")}catch(m){f=new URL("/","http://n")}try{const e=new URL(d,f);e.pathname=(0,u.normalizePathTrailingSlash)(e.pathname);let t="";if((0,l.isDynamicRoute)(e.pathname)&&e.searchParams&&n){const n=(0,r.searchParamsToUrlQuery)(e.searchParams),{result:i,params:u}=(0,c.interpolateAs)(e.pathname,e.pathname,n);i&&(t=(0,o.formatWithValidation)({pathname:i,hash:e.hash,query:(0,a.omit)(n,u)}))}const i=e.origin===f.origin?e.href.slice(e.origin.length):e.href;return n?[i,t||i]:i}catch(m){return n?[d]:d}}("function"===typeof t.default||"object"===typeof t.default&&null!==t.default)&&"undefined"===typeof t.default.__esModule&&(Object.defineProperty(t.default,"__esModule",{value:!0}),Object.assign(t.default,t),e.exports=t.default)},3868:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"useIntersection",{enumerable:!0,get:function(){return l}});const r=n(9585),o=n(6790),a="function"===typeof IntersectionObserver,i=new Map,u=[];function s(e,t,n){const{id:r,observer:o,elements:a}=function(e){const t={root:e.root||null,margin:e.rootMargin||""},n=u.find((e=>e.root===t.root&&e.margin===t.margin));let r;if(n&&(r=i.get(n),r))return r;const o=new Map,a=new IntersectionObserver((e=>{e.forEach((e=>{const t=o.get(e.target),n=e.isIntersecting||e.intersectionRatio>0;t&&n&&t(n)}))}),e);return r={id:t,observer:a,elements:o},u.push(t),i.set(t,r),r}(n);return a.set(e,t),o.observe(e),function(){if(a.delete(e),o.unobserve(e),0===a.size){o.disconnect(),i.delete(r);const e=u.findIndex((e=>e.root===r.root&&e.margin===r.margin));e>-1&&u.splice(e,1)}}}function l(e){let{rootRef:t,rootMargin:n,disabled:i}=e;const u=i||!a,[l,c]=(0,r.useState)(!1),f=(0,r.useRef)(null),d=(0,r.useCallback)((e=>{f.current=e}),[]);(0,r.useEffect)((()=>{if(a){if(u||l)return;const e=f.current;if(e&&e.tagName){return s(e,(e=>e&&c(e)),{root:null==t?void 0:t.current,rootMargin:n})}}else if(!l){const e=(0,o.requestIdleCallback)((()=>c(!0)));return()=>(0,o.cancelIdleCallback)(e)}}),[u,n,t,l,f.current]);const p=(0,r.useCallback)((()=>{c(!1)}),[]);return[d,l,p]}("function"===typeof t.default||"object"===typeof t.default&&null!==t.default)&&"undefined"===typeof t.default.__esModule&&(Object.defineProperty(t.default,"__esModule",{value:!0}),Object.assign(t.default,t),e.exports=t.default)},8161:function(e,t){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"escapeStringRegexp",{enumerable:!0,get:function(){return o}});const n=/[|\\{}()[\]^$+*?.-]/,r=/[|\\{}()[\]^$+*?.-]/g;function o(e){return n.test(e)?e.replace(r,"\\$&"):e}},1917:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"RouterContext",{enumerable:!0,get:function(){return r}});const r=n(7666)._(n(9585)).default.createContext(null)},3570:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),function(e,t){for(var n in t)Object.defineProperty(e,n,{enumerable:!0,get:t[n]})}(t,{formatUrl:function(){return a},formatWithValidation:function(){return u},urlObjectKeys:function(){return i}});const r=n(3520)._(n(6537)),o=/https?|ftp|gopher|file/;function a(e){let{auth:t,hostname:n}=e,a=e.protocol||"",i=e.pathname||"",u=e.hash||"",s=e.query||"",l=!1;t=t?encodeURIComponent(t).replace(/%3A/i,":")+"@":"",e.host?l=t+e.host:n&&(l=t+(~n.indexOf(":")?"["+n+"]":n),e.port&&(l+=":"+e.port)),s&&"object"===typeof s&&(s=String(r.urlQueryToSearchParams(s)));let c=e.search||s&&"?"+s||"";return a&&!a.endsWith(":")&&(a+=":"),e.slashes||(!a||o.test(a))&&!1!==l?(l="//"+(l||""),i&&"/"!==i[0]&&(i="/"+i)):l||(l=""),u&&"#"!==u[0]&&(u="#"+u),c&&"?"!==c[0]&&(c="?"+c),i=i.replace(/[?#]/g,encodeURIComponent),c=c.replace("#","%23"),""+a+l+i+c+u}const i=["auth","hash","host","hostname","href","path","pathname","port","protocol","query","search","slashes"];function u(e){return a(e)}},7145:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),function(e,t){for(var n in t)Object.defineProperty(e,n,{enumerable:!0,get:t[n]})}(t,{getSortedRoutes:function(){return r.getSortedRoutes},isDynamicRoute:function(){return o.isDynamicRoute}});const r=n(3963),o=n(9475)},3929:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"interpolateAs",{enumerable:!0,get:function(){return a}});const r=n(1889),o=n(1019);function a(e,t,n){let a="";const i=(0,o.getRouteRegex)(e),u=i.groups,s=(t!==e?(0,r.getRouteMatcher)(i)(t):"")||n;a=e;const l=Object.keys(u);return l.every((e=>{let t=s[e]||"";const{repeat:n,optional:r}=u[e];let o="["+(n?"...":"")+e+"]";return r&&(o=(t?"":"/")+"["+o+"]"),n&&!Array.isArray(t)&&(t=[t]),(r||e in s)&&(a=a.replace(o,n?t.map((e=>encodeURIComponent(e))).join("/"):encodeURIComponent(t))||"/")}))||(a=""),{params:l,result:a}}},9475:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"isDynamicRoute",{enumerable:!0,get:function(){return a}});const r=n(3468),o=/\/\[[^/]+?\](?=\/|$)/;function a(e){return(0,r.isInterceptionRouteAppPath)(e)&&(e=(0,r.extractInterceptionRouteInformation)(e).interceptedRoute),o.test(e)}},5499:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"isLocalURL",{enumerable:!0,get:function(){return a}});const r=n(6e3),o=n(3655);function a(e){if(!(0,r.isAbsoluteUrl)(e))return!0;try{const t=(0,r.getLocationOrigin)(),n=new URL(e,t);return n.origin===t&&(0,o.hasBasePath)(n.pathname)}catch(t){return!1}}},9084:function(e,t){function n(e,t){const n={};return Object.keys(e).forEach((r=>{t.includes(r)||(n[r]=e[r])})),n}Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"omit",{enumerable:!0,get:function(){return n}})},6537:function(e,t){function n(e){const t={};return e.forEach(((e,n)=>{"undefined"===typeof t[n]?t[n]=e:Array.isArray(t[n])?t[n].push(e):t[n]=[t[n],e]})),t}function r(e){return"string"===typeof e||"number"===typeof e&&!isNaN(e)||"boolean"===typeof e?String(e):""}function o(e){const t=new URLSearchParams;return Object.entries(e).forEach((e=>{let[n,o]=e;Array.isArray(o)?o.forEach((e=>t.append(n,r(e)))):t.set(n,r(o))})),t}function a(e){for(var t=arguments.length,n=new Array(t>1?t-1:0),r=1;r<t;r++)n[r-1]=arguments[r];return n.forEach((t=>{Array.from(t.keys()).forEach((t=>e.delete(t))),t.forEach(((t,n)=>e.append(n,t)))})),e}Object.defineProperty(t,"__esModule",{value:!0}),function(e,t){for(var n in t)Object.defineProperty(e,n,{enumerable:!0,get:t[n]})}(t,{assign:function(){return a},searchParamsToUrlQuery:function(){return n},urlQueryToSearchParams:function(){return o}})},1889:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"getRouteMatcher",{enumerable:!0,get:function(){return o}});const r=n(6e3);function o(e){let{re:t,groups:n}=e;return e=>{const o=t.exec(e);if(!o)return!1;const a=e=>{try{return decodeURIComponent(e)}catch(t){throw new r.DecodeError("failed to decode param")}},i={};return Object.keys(n).forEach((e=>{const t=n[e],r=o[t.pos];void 0!==r&&(i[e]=~r.indexOf("/")?r.split("/").map((e=>a(e))):t.repeat?[a(r)]:a(r))})),i}}},1019:function(e,t,n){Object.defineProperty(t,"__esModule",{value:!0}),function(e,t){for(var n in t)Object.defineProperty(e,n,{enumerable:!0,get:t[n]})}(t,{getNamedMiddlewareRegex:function(){return h},getNamedRouteRegex:function(){return p},getRouteRegex:function(){return c}});const r=n(3468),o=n(8161),a=n(74),i="nxtP",u="nxtI";function s(e){const t=e.startsWith("[")&&e.endsWith("]");t&&(e=e.slice(1,-1));const n=e.startsWith("...");return n&&(e=e.slice(3)),{key:e,repeat:n,optional:t}}function l(e){const t=(0,a.removeTrailingSlash)(e).slice(1).split("/"),n={};let i=1;return{parameterizedRoute:t.map((e=>{const t=r.INTERCEPTION_ROUTE_MARKERS.find((t=>e.startsWith(t))),a=e.match(/\[((?:\[.*\])|.+)\]/);if(t&&a){const{key:e,optional:r,repeat:u}=s(a[1]);return n[e]={pos:i++,repeat:u,optional:r},"/"+(0,o.escapeStringRegexp)(t)+"([^/]+?)"}if(a){const{key:e,repeat:t,optional:r}=s(a[1]);return n[e]={pos:i++,repeat:t,optional:r},t?r?"(?:/(.+?))?":"/(.+?)":"/([^/]+?)"}return"/"+(0,o.escapeStringRegexp)(e)})).join(""),groups:n}}function c(e){const{parameterizedRoute:t,groups:n}=l(e);return{re:new RegExp("^"+t+"(?:/)?$"),groups:n}}function f(e){let{interceptionMarker:t,getSafeRouteKey:n,segment:r,routeKeys:a,keyPrefix:i}=e;const{key:u,optional:l,repeat:c}=s(r);let f=u.replace(/\W/g,"");i&&(f=""+i+f);let d=!1;(0===f.length||f.length>30)&&(d=!0),isNaN(parseInt(f.slice(0,1)))||(d=!0),d&&(f=n()),a[f]=i?""+i+u:u;const p=t?(0,o.escapeStringRegexp)(t):"";return c?l?"(?:/"+p+"(?<"+f+">.+?))?":"/"+p+"(?<"+f+">.+?)":"/"+p+"(?<"+f+">[^/]+?)"}function d(e,t){const n=(0,a.removeTrailingSlash)(e).slice(1).split("/"),s=function(){let e=0;return()=>{let t="",n=++e;for(;n>0;)t+=String.fromCharCode(97+(n-1)%26),n=Math.floor((n-1)/26);return t}}(),l={};return{namedParameterizedRoute:n.map((e=>{const n=r.INTERCEPTION_ROUTE_MARKERS.some((t=>e.startsWith(t))),a=e.match(/\[((?:\[.*\])|.+)\]/);if(n&&a){const[n]=e.split(a[0]);return f({getSafeRouteKey:s,interceptionMarker:n,segment:a[1],routeKeys:l,keyPrefix:t?u:void 0})}return a?f({getSafeRouteKey:s,segment:a[1],routeKeys:l,keyPrefix:t?i:void 0}):"/"+(0,o.escapeStringRegexp)(e)})).join(""),routeKeys:l}}function p(e,t){const n=d(e,t);return{...c(e),namedRegex:"^"+n.namedParameterizedRoute+"(?:/)?$",routeKeys:n.routeKeys}}function h(e,t){const{parameterizedRoute:n}=l(e),{catchAll:r=!0}=t;if("/"===n){return{namedRegex:"^/"+(r?".*":"")+"$"}}const{namedParameterizedRoute:o}=d(e,!1);return{namedRegex:"^"+o+(r?"(?:(/.*)?)":"")+"$"}}},3963:function(e,t){Object.defineProperty(t,"__esModule",{value:!0}),Object.defineProperty(t,"getSortedRoutes",{enumerable:!0,get:function(){return r}});class n{insert(e){this._insert(e.split("/").filter(Boolean),[],!1)}smoosh(){return this._smoosh()}_smoosh(e){void 0===e&&(e="/");const t=[...this.children.keys()].sort();null!==this.slugName&&t.splice(t.indexOf("[]"),1),null!==this.restSlugName&&t.splice(t.indexOf("[...]"),1),null!==this.optionalRestSlugName&&t.splice(t.indexOf("[[...]]"),1);const n=t.map((t=>this.children.get(t)._smoosh(""+e+t+"/"))).reduce(((e,t)=>[...e,...t]),[]);if(null!==this.slugName&&n.push(...this.children.get("[]")._smoosh(e+"["+this.slugName+"]/")),!this.placeholder){const t="/"===e?"/":e.slice(0,-1);if(null!=this.optionalRestSlugName)throw new Error('You cannot define a route with the same specificity as a optional catch-all route ("'+t+'" and "'+t+"[[..."+this.optionalRestSlugName+']]").');n.unshift(t)}return null!==this.restSlugName&&n.push(...this.children.get("[...]")._smoosh(e+"[..."+this.restSlugName+"]/")),null!==this.optionalRestSlugName&&n.push(...this.children.get("[[...]]")._smoosh(e+"[[..."+this.optionalRestSlugName+"]]/")),n}_insert(e,t,r){if(0===e.length)return void(this.placeholder=!1);if(r)throw new Error("Catch-all must be the last part of the URL.");let o=e[0];if(o.startsWith("[")&&o.endsWith("]")){let a=o.slice(1,-1),i=!1;if(a.startsWith("[")&&a.endsWith("]")&&(a=a.slice(1,-1),i=!0),a.startsWith("...")&&(a=a.substring(3),r=!0),a.startsWith("[")||a.endsWith("]"))throw new Error("Segment names may not start or end with extra brackets ('"+a+"').");if(a.startsWith("."))throw new Error("Segment names may not start with erroneous periods ('"+a+"').");function u(e,n){if(null!==e&&e!==n)throw new Error("You cannot use different slug names for the same dynamic path ('"+e+"' !== '"+n+"').");t.forEach((e=>{if(e===n)throw new Error('You cannot have the same slug name "'+n+'" repeat within a single dynamic path');if(e.replace(/\W/g,"")===o.replace(/\W/g,""))throw new Error('You cannot have the slug names "'+e+'" and "'+n+'" differ only by non-word symbols within a single dynamic path')})),t.push(n)}if(r)if(i){if(null!=this.restSlugName)throw new Error('You cannot use both an required and optional catch-all route at the same level ("[...'+this.restSlugName+']" and "'+e[0]+'" ).');u(this.optionalRestSlugName,a),this.optionalRestSlugName=a,o="[[...]]"}else{if(null!=this.optionalRestSlugName)throw new Error('You cannot use both an optional and required catch-all route at the same level ("[[...'+this.optionalRestSlugName+']]" and "'+e[0]+'").');u(this.restSlugName,a),this.restSlugName=a,o="[...]"}else{if(i)throw new Error('Optional route parameters are not yet supported ("'+e[0]+'").');u(this.slugName,a),this.slugName=a,o="[]"}}this.children.has(o)||this.children.set(o,new n),this.children.get(o)._insert(e.slice(1),t,r)}constructor(){this.placeholder=!0,this.children=new Map,this.slugName=null,this.restSlugName=null,this.optionalRestSlugName=null}}function r(e){const t=new n;return e.forEach((e=>t.insert(e))),t.smoosh()}},6e3:function(e,t){Object.defineProperty(t,"__esModule",{value:!0}),function(e,t){for(var n in t)Object.defineProperty(e,n,{enumerable:!0,get:t[n]})}(t,{DecodeError:function(){return h},MiddlewareNotFoundError:function(){return b},MissingStaticPage:function(){return y},NormalizeError:function(){return m},PageNotFoundError:function(){return g},SP:function(){return d},ST:function(){return p},WEB_VITALS:function(){return n},execOnce:function(){return r},getDisplayName:function(){return s},getLocationOrigin:function(){return i},getURL:function(){return u},isAbsoluteUrl:function(){return a},isResSent:function(){return l},loadGetInitialProps:function(){return f},normalizeRepeatedSlashes:function(){return c},stringifyError:function(){return P}});const n=["CLS","FCP","FID","INP","LCP","TTFB"];function r(e){let t,n=!1;return function(){for(var r=arguments.length,o=new Array(r),a=0;a<r;a++)o[a]=arguments[a];return n||(n=!0,t=e(...o)),t}}const o=/^[a-zA-Z][a-zA-Z\d+\-.]*?:/,a=e=>o.test(e);function i(){const{protocol:e,hostname:t,port:n}=window.location;return e+"//"+t+(n?":"+n:"")}function u(){const{href:e}=window.location,t=i();return e.substring(t.length)}function s(e){return"string"===typeof e?e:e.displayName||e.name||"Unknown"}function l(e){return e.finished||e.headersSent}function c(e){const t=e.split("?");return t[0].replace(/\\/g,"/").replace(/\/\/+/g,"/")+(t[1]?"?"+t.slice(1).join("?"):"")}async function f(e,t){const n=t.res||t.ctx&&t.ctx.res;if(!e.getInitialProps)return t.ctx&&t.Component?{pageProps:await f(t.Component,t.ctx)}:{};const r=await e.getInitialProps(t);if(n&&l(n))return r;if(!r){const t='"'+s(e)+'.getInitialProps()" should resolve to an object. But found "'+r+'" instead.';throw new Error(t)}return r}const d="undefined"!==typeof performance,p=d&&["mark","measure","getEntriesByName"].every((e=>"function"===typeof performance[e]));class h extends Error{}class m extends Error{}class g extends Error{constructor(e){super(),this.code="ENOENT",this.name="PageNotFoundError",this.message="Cannot find module for page: "+e}}class y extends Error{constructor(e,t){super(),this.message="Failed to load static file for page: "+e+" "+t}}class b extends Error{constructor(){super(),this.code="ENOENT",this.message="Cannot find the middleware module"}}function P(e){return JSON.stringify({message:e.message,stack:e.stack})}}}]);