"use strict";(self.webpackChunkmovies=self.webpackChunkmovies||[]).push([["common"],{9456:(I,f,s)=>{s.d(f,{H:()=>m});var n=s(6719),M=s(7782),r=s(3075),l=s(5879),c=s(9862);const D=[M.w,"discover","movie"].join("/");function E(h){const{with_cast:O,with_genres:u,...v}=h,L={...(0,n.K)(v),...(0,r.C)(v)};return O&&(L.with_cast=O),u&&(L.with_genres=u),L}let m=(()=>{class h{constructor(){this.http=(0,l.f3M)(c.eN),this.getDiscoverMovies=(u={})=>this.http.get(D,{params:new c.LE({fromObject:E(u)})})}static#t=this.\u0275fac=function(v){return new(v||h)};static#s=this.\u0275prov=l.Yz7({token:h,factory:h.\u0275fac,providedIn:"root"})}return h})()},2951:(I,f,s)=>{s.d(f,{XC:()=>P});var n=s(5879),M=s(1321),r=s(7979),l=s(7398),c=s(2460),D=s(4664),E=s(7921),m=s(4082),h=s(8312),O=s(1520),u=s(8202),v=s(9699),L=s(7344);let P=(()=>{class i extends M.x{constructor(){super(),this.listState=(0,n.f3M)(O.V),this.routerState=(0,n.f3M)(u.u),this.ui=(new h.AE).create(),this.srcset="154w, 185w, 342w, 500w, 780w",this.routerListId$=this.routerState.select((0,l.U)(t=>t?.type)),this.listInfoUpdateEvent$=this.ui.listInfoUpdate$.pipe((0,c.M)(this.select("id"))),this.listPosterUpdateEvent$=this.ui.listPosterUpdate$.pipe((0,c.M)(this.select("id"))),this.listDeleteEvent$=this.ui.deleteList$.pipe((0,c.M)(this.select("id"))),this.listDetails$=this.select("id").pipe((0,D.w)(t=>this.listState.select("lists",t))),this.movies$=this.listDetails$.pipe((0,r.Ys)("results"),(0,l.U)(t=>void 0!==t?t.map(d):[])),this.posters$=this.listDetails$.pipe((0,r.Cc)(["results","backdrop_path"]),(0,l.U)(({results:t,backdrop_path:e})=>t?.map(g=>({...(0,v.z)(g,{pathProp:"backdrop_path",dims:m.J1,fallback:L.mw}),selected:g.backdrop_path===e})))),this.listName$=this.listDetails$.pipe((0,r.Ys)("name"),(0,E.O)("Loading...")),this.connect("id",this.routerListId$),this.hold(this.routerListId$,this.listState.fetchList),this.hold(this.listInfoUpdateEvent$,([t,e])=>this.listState.updateList({...t,id:+e})),this.hold(this.listPosterUpdateEvent$,([t,e])=>this.listState.updateList({backdrop_path:t,id:+e})),this.hold(this.listDeleteEvent$,([,t])=>this.listState.deleteList(t))}static#t=this.\u0275fac=function(e){return new(e||i)};static#s=this.\u0275prov=n.Yz7({token:i,factory:i.\u0275fac,providedIn:"root"})}return i})();function d(i){return(0,v.z)(i,{pathProp:"poster_path",dims:m.SU,sizes:"(min-width: 900px) 20vw, 70vw",srcset:"154w, 185w, 342w, 500w, 780w"})}},4826:(I,f,s)=>{s.d(f,{h:()=>v});var n=s(1102),M=s(7398),r=s(9360),l=s(8251),D=s(5211),E=s(6328),m=s(6232),h=s(6699),O=s(1017),u=s(4369);function v(L,P,o={}){let p=0,d=2;const i=(0,u.U)(o).pipe((0,M.U)(a=>{const{page:t,total_pages:e,...g}=a;return p=t||p,d=e||d,{page:p,total_pages:d,...g}}),function c(L,P=!1){return(0,r.e)((o,p)=>{let d=0;o.subscribe((0,l.x)(p,i=>{const a=L(i,d++);(a||P)&&p.next(i),!a&&p.complete()}))})}(a=>!Array.isArray(a.results),!0));return(0,D.z)(i.pipe((0,O.Q)()),P.pipe((0,E.b)(()=>(++p,p<=d?L({page:p}).pipe((0,O.Q)()):m.E)))).pipe((0,h.R)((a,t)=>(t?.total_pages&&(d=t.total_pages),t?.results&&(a.results=(0,n.$T)(a?.results,t?.results||[]),delete t.results),(0,n.r$)(a,t)),{page:p,total_pages:d}))}},485:(I,f,s)=>{s.d(f,{m:()=>P});var n=s(5879),M=s(1321),r=s(7398),l=s(2181),c=s(4664),D=s(6719),E=s(7782),m=s(3075),h=s(9862);function u(o){return{...(0,D.K)(o),...(0,m.C)(o)}}let v=(()=>{class o{constructor(){this.http=(0,n.f3M)(h.eN),this.getAccountList=(d,i={})=>this.http.get((o=>[E.n,"account",o,"lists"].join("/"))(d),{params:new h.LE({fromObject:u(i)})})}static#t=this.\u0275fac=function(i){return new(i||o)};static#s=this.\u0275prov=n.Yz7({token:o,factory:o.\u0275fac,providedIn:"root"})}return o})();var L=s(6814);let P=(()=>{class o extends M.x{constructor(){super(),this.platformId=(0,n.f3M)(n.Lbi),this.accountId$=this.select("accountId"),this.loggedIn$=this.select((0,r.U)(({accountId:i})=>null!==i)),this.accountLists$=this.select("lists");const d=(0,n.f3M)(v);(0,L.NF)(this.platformId)&&this.set({accountId:window.localStorage.getItem("accountId")}),this.connect("lists",this.accountId$.pipe((0,l.h)(i=>null!==i),(0,c.w)(i=>d.getAccountList(i).pipe((0,r.U)(({results:a})=>a)))))}static#t=this.\u0275fac=function(i){return new(i||o)};static#s=this.\u0275prov=n.Yz7({token:o,factory:o.\u0275fac,providedIn:"root"})}return o})()},1520:(I,f,s)=>{s.d(f,{V:()=>d});var n=s(5879),M=s(1321),r=s(3019),l=s(6328),c=s(9397),D=s(2181),E=s(7398),m=s(7782),h=s(9862);const O=[m.n,"list"].join("/"),u=i=>[O,i].join("/"),v=i=>[u(i),"items"].join("/");let L=(()=>{class i{constructor(){this.http=(0,n.f3M)(h.eN),this.createList=t=>this.http.post(O,t).pipe((0,E.U)(({id:e})=>e)),this.fetchList=t=>this.http.get(u(+t)).pipe((0,E.U)(e=>({[t]:e}))),this.updateList=t=>this.http.put(u(t.id||0),t),this.addMovieToList=t=>this.http.post(v(t.id),t),this.deleteMovieFromList=t=>this.http.delete(v(t.id),{body:t}),this.deleteList=t=>this.http.delete(u(+t))}static#t=this.\u0275fac=function(e){return new(e||i)};static#s=this.\u0275prov=n.Yz7({token:i,factory:i.\u0275fac,providedIn:"root"})}return i})();var P=s(4190),o=s(1102),p=s(8312);let d=(()=>{class i extends M.x{constructor(){(0,n.f3M)(n.ktI).onDestroy(()=>this.actionsF.destroy()),super(),this.router=(0,n.f3M)(P.F0),this.actionsF=new p.AE,this.listResource=(0,n.f3M)(L),this.actions=this.actionsF.create(),this.createList=this.actions.createList,this.fetchList=this.actions.fetchList,this.updateList=this.actions.updateList,this.addMovieToList=this.actions.addMovieToList,this.deleteMovieFromList=this.actions.deleteMovieFromList,this.deleteList=this.actions.deleteList,this.deleteListSignal$=this.actions.deleteList$,this.sideEffects$=(0,r.T)(this.actions.addMovieToList$.pipe((0,l.b)(([t,e])=>this.listResource.addMovieToList({id:e,items:[{media_id:t.id,media_type:"movie"}]}))),this.actions.deleteMovieFromList$.pipe((0,l.b)(([t,e])=>this.listResource.deleteMovieFromList({id:e,items:[{media_id:t.id||0,media_type:"movie"}]}))),this.actions.updateList$.pipe((0,l.b)(t=>this.listResource.updateList(t))),this.actions.createList$.pipe((0,l.b)(t=>this.listResource.createList(t)),(0,c.b)(t=>t&&this.router.navigate(["account/my-lists"]))),this.actions.deleteList$.pipe((0,c.b)(t=>t&&this.router.navigate(["account/my-lists"])),(0,l.b)(t=>this.listResource.deleteList(t)))),this.connect("lists",this.actions.fetchList$.pipe((0,D.h)(t=>!isNaN(Number(t))),(0,l.b)(t=>this.listResource.fetchList(t))),(t,e)=>(0,o.r$)(t?.lists||{},e)),this.connect("lists",this.actions.updateList$,(t,e)=>t&&e.id?(0,o.r$)(t.lists,{[e.id]:(0,o.r$)(t.lists[e.id],e)}):t.lists),this.connect("lists",this.actions.addMovieToList$,(t,[e,g])=>t&&g?(0,o.r$)(t.lists,{[g]:(0,o.r$)(t.lists[g],{results:[...t.lists[g].results||[],e]})}):t.lists),this.connect("lists",this.actions.deleteList$,(t,e)=>t&&e?(0,o.fL)(t.lists,`${e}`):t.lists),this.connect("lists",this.actions.deleteMovieFromList$,(t,[e,g])=>t&&g?(0,o.r$)(t.lists,{[g]:(0,o.r$)(t.lists[g],{results:(t.lists[g].results||[]).filter($=>$.id!==e.id)})}):t.lists),this.hold(this.sideEffects$)}initialize(){}static#t=this.\u0275fac=function(e){return new(e||i)};static#s=this.\u0275prov=n.Yz7({token:i,factory:i.\u0275fac,providedIn:"root"})}return i})()},3251:(I,f,s)=>{s.d(f,{K:()=>l});var n=s(5879);const M=[[["","detailGridMedia",""]],[["","detailGridDescription",""]]],r=["[detailGridMedia]","[detailGridDescription]"];let l=(()=>{class c{static#t=this.\u0275fac=function(m){return new(m||c)};static#s=this.\u0275cmp=n.Xpm({type:c,selectors:[["ui-detail-grid"]],standalone:!0,features:[n.jDz],ngContentSelectors:r,decls:4,vars:0,consts:[[1,"grid--item","gradient"],[1,"grid--item","media"]],template:function(m,h){1&m&&(n.F$t(M),n.TgZ(0,"div",0),n.Hsn(1),n.qZA(),n.TgZ(2,"div",1),n.Hsn(3,1),n.qZA())},styles:['.gradient[_ngcontent-%COMP%]{position:relative}.gradient[_ngcontent-%COMP%]:after{content:"";position:absolute;left:0;top:0;width:100%;height:100%;display:inline-block;background-image:var(--background-blend-gradient)}[_nghost-%COMP%]{display:grid;grid-template-columns:40% 60%;max-width:120rem;margin:0 auto 7rem}@media only screen and (max-width: 900px){[_nghost-%COMP%]{display:block;grid-template-columns:unset;margin-bottom:5rem}}@media only screen and (max-width: 1300px){[_nghost-%COMP%]{max-width:110rem;margin-bottom:5rem}[_nghost-%COMP%]   .grid--item[_ngcontent-%COMP%]{padding:2rem}}@media only screen and (max-width: 1462.5px){[_nghost-%COMP%]{max-width:110rem;margin-bottom:6rem}}@media only screen and (max-width: 1500px){[_nghost-%COMP%]{max-width:105rem}}.grid--item[_ngcontent-%COMP%]{padding:4rem}'],changeDetection:0})}return c})()},7728:(I,f,s)=>{s.d(f,{k:()=>M});const{isArray:n}=Array;function M(r){return 1===r.length&&n(r[0])?r[0]:r}}}]);