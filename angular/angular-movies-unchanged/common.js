"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["common"],{

/***/ 5746:
/*!*******************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/account.resource.ts ***!
  \*******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   AccountResource: () => (/* binding */ AccountResource)
/* harmony export */ });
/* harmony import */ var _paginate_utils__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../paginate/utils */ 6719);
/* harmony import */ var _internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./internal/base-urls.constant */ 7782);
/* harmony import */ var _sort_utils__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../sort/utils */ 3075);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/common/http */ 4860);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/core */ 1699);






const URL_ACCOUNT_LIST = uid => [_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_1__.baseUrlApiV4, 'account', uid, 'lists'].join('/');
function getTMDBAcountListOptions(options) {
  const discoverOptions = {
    ...(0,_paginate_utils__WEBPACK_IMPORTED_MODULE_0__.getTMDBPaginateOptions)(options),
    ...(0,_sort_utils__WEBPACK_IMPORTED_MODULE_2__.getTMDBSortOptions)(options)
  };
  return discoverOptions;
}
class AccountResource {
  constructor() {
    this.http = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_angular_common_http__WEBPACK_IMPORTED_MODULE_4__.HttpClient);
    this.getAccountList = (accountId, params = {}) => {
      return this.http.get(URL_ACCOUNT_LIST(accountId), {
        params: new _angular_common_http__WEBPACK_IMPORTED_MODULE_4__.HttpParams({
          fromObject: getTMDBAcountListOptions(params)
        })
      });
    };
  }
  static #_ = this.ɵfac = function AccountResource_Factory(t) {
    return new (t || AccountResource)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵdefineInjectable"]({
    token: AccountResource,
    factory: AccountResource.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 9456:
/*!********************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/discover.resource.ts ***!
  \********************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   DiscoverResource: () => (/* binding */ DiscoverResource)
/* harmony export */ });
/* harmony import */ var _paginate_utils__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../paginate/utils */ 6719);
/* harmony import */ var _internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./internal/base-urls.constant */ 7782);
/* harmony import */ var _sort_utils__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../sort/utils */ 3075);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/common/http */ 4860);






const URL_DISCOVER_MOVIE = [_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_1__.baseUrlApiV3, 'discover', 'movie'].join('/');
function getTMDBDiscoverOptions(options) {
  const {
    with_cast,
    with_genres,
    ...tmdbOptions
  } = options;
  const discoverOptions = {
    ...(0,_paginate_utils__WEBPACK_IMPORTED_MODULE_0__.getTMDBPaginateOptions)(tmdbOptions),
    ...(0,_sort_utils__WEBPACK_IMPORTED_MODULE_2__.getTMDBSortOptions)(tmdbOptions)
  };
  with_cast && (discoverOptions.with_cast = with_cast);
  with_genres && (discoverOptions.with_genres = with_genres);
  return discoverOptions;
}
class DiscoverResource {
  constructor() {
    this.http = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_angular_common_http__WEBPACK_IMPORTED_MODULE_4__.HttpClient);
    /**
     * This endpoint returns related movies for genres and cast actors
     * @param discoverOptions
     */
    this.getDiscoverMovies = (discoverOptions = {}) => this.http.get(URL_DISCOVER_MOVIE, {
      params: new _angular_common_http__WEBPACK_IMPORTED_MODULE_4__.HttpParams({
        fromObject: getTMDBDiscoverOptions(discoverOptions)
      })
    });
  }
  static #_ = this.ɵfac = function DiscoverResource_Factory(t) {
    return new (t || DiscoverResource)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵdefineInjectable"]({
    token: DiscoverResource,
    factory: DiscoverResource.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 8762:
/*!******************************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/infinite-scroll/infiniteScroll.ts ***!
  \******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   infiniteScroll: () => (/* binding */ infiniteScroll)
/* harmony export */ });
/* harmony import */ var _rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @rx-angular/cdk/transformations */ 3751);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 2541);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! rxjs */ 9644);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! rxjs */ 9877);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! rxjs */ 6290);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 623);
/* harmony import */ var _loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../loading/withLoadingEmissions */ 9135);
/* harmony import */ var _coerceObservable__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../coerceObservable */ 4369);




/**
 *
 * A helper function to trigger HTTP requests on a paginated API.
 *
 * @example
 *
 * // To paginate a resource we need at minimal 2 things, the page number and the total pages.
 *
 * // Let's say you want to paginate the following http request:
 *  fetchList(id: number, pageNumber: number): Observable<{result: any[], total_pages: number}>
 *
 * // We want to get the result under the name `list`, not `results` as the APi provides.
 * // In this function the active page is hidden but the total pages is needed and named `totalPages`, so we need to map it too
 * const mapToPaginationResult = () => map(({ results, total_pages }) => ({list: results, totalPages: total_pages}))
 *
 * // And following trigger to fetch the new page:
 * load$: Observable<void>;
 *
 * // the implementation for a static id would lok like this:
 * const activeListId = 42;
 *
 * const paginated$: Observable<{list: any[]} & PaginatedState> =
 *    infiniteScrolled(
 *      (options, triggerID) => fetchList(triggerID, options.page).pipe(mapToPaginationResult()),
 *     load$.pipe(mapTo(activeListId)),
 *     // optional start page, default 0, e.g. {page: 4}, of({page: 4, result: any[]})
 *     // Notice: If an Observable is passed the result will also be emitted as first value.
 *    )
 * );
 *
 * @example
 * const infiniteList$ = infiniteScrolled(
 *   (options) => getDiscoverMovies(identifier, options),
 *   this.actions.paginate$,
 *   getDiscoverMovies(identifier, { page: 1 })
 * );
 *
 */
function infiniteScroll(fetchFn, trigger$, initialPageOrLastResult = {}) {
  let page = 0;
  let total_pages = 2;
  // We need to reduce the initial page by one as we start by incrementing it
  const initialResult$ = (0,_coerceObservable__WEBPACK_IMPORTED_MODULE_1__.coerceObservable)(initialPageOrLastResult).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_2__.map)(s => {
    const {
      page: _page,
      total_pages: _total_pages,
      ...rest
    } = s;
    // if no start result is given start with page 0, total pages 2 => next request will be page 1
    // if no initial result is given start with an empty list
    page = _page || page;
    total_pages = _total_pages || total_pages;
    return {
      page,
      total_pages,
      ...rest
    };
  }),
  // in case there is global state connected we take care of just taking the initial value that includes a result.
  // loading emissions are forwarded as they are and merged into the result stream. This "forwards" the possible inflight state of the initial result.
  (0,rxjs__WEBPACK_IMPORTED_MODULE_3__.takeWhile)(r => !Array.isArray(r.results), true));
  return (0,rxjs__WEBPACK_IMPORTED_MODULE_4__.concat)(initialResult$.pipe((0,_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_0__.withLoadingEmission)()), trigger$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_5__.concatMap)(() => {
    ++page;
    return page <= total_pages ? fetchFn({
      page
    }).pipe((0,_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_0__.withLoadingEmission)()) : rxjs__WEBPACK_IMPORTED_MODULE_6__.EMPTY;
  }))).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.scan)((acc, response) => {
    // in case the initial value was no set we take total pages from the result
    if (response?.total_pages) {
      total_pages = response.total_pages;
    }
    // Only treas results if they are given.
    // Avoid emitting unnecessary empty arrays which cause render filcker and bad performance
    if (response?.results) {
      acc.results = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__.insert)(acc?.results, response?.results || []);
      delete response.results;
    }
    // the rest gets updated
    return (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__.patch)(acc, response);
  }, {
    page,
    total_pages
  }));
}

/***/ }),

/***/ 3412:
/*!********************************************************!*\
  !*** ./projects/movies/src/app/state/account.state.ts ***!
  \********************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   AccountState: () => (/* binding */ AccountState)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! rxjs */ 1891);
/* harmony import */ var _data_access_api_resources_account_resource__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../data-access/api/resources/account.resource */ 5746);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/common */ 6575);






class AccountState extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__.RxState {
  constructor() {
    super();
    this.platformId = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_2__.PLATFORM_ID);
    this.accountId$ = this.select('accountId');
    this.loggedIn$ = this.select((0,rxjs__WEBPACK_IMPORTED_MODULE_3__.map)(({
      accountId
    }) => accountId !== null));
    this.accountLists$ = this.select('lists');
    const authResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_data_access_api_resources_account_resource__WEBPACK_IMPORTED_MODULE_0__.AccountResource);
    if ((0,_angular_common__WEBPACK_IMPORTED_MODULE_4__.isPlatformBrowser)(this.platformId)) {
      // set accountId if found in localStorage
      this.set({
        accountId: window.localStorage.getItem('accountId')
      });
    }
    this.connect('lists', this.accountId$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_5__.filter)(accountId => accountId !== null), (0,rxjs__WEBPACK_IMPORTED_MODULE_6__.switchMap)(id => authResource.getAccountList(id).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_3__.map)(({
      results
    }) => results)))));
  }
  static #_ = this.ɵfac = function AccountState_Factory(t) {
    return new (t || AccountState)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineInjectable"]({
    token: AccountState,
    factory: AccountState.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 3251:
/*!***********************************************************************************!*\
  !*** ./projects/movies/src/app/ui/component/detail-grid/detail-grid.component.ts ***!
  \***********************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   DetailGridComponent: () => (/* binding */ DetailGridComponent)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);

const _c0 = [[["", "detailGridMedia", ""]], [["", "detailGridDescription", ""]]];
const _c1 = ["[detailGridMedia]", "[detailGridDescription]"];
class DetailGridComponent {
  static #_ = this.ɵfac = function DetailGridComponent_Factory(t) {
    return new (t || DetailGridComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineComponent"]({
    type: DetailGridComponent,
    selectors: [["ui-detail-grid"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵStandaloneFeature"]],
    ngContentSelectors: _c1,
    decls: 4,
    vars: 0,
    consts: [[1, "grid--item", "gradient"], [1, "grid--item", "media"]],
    template: function DetailGridComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojectionDef"](_c0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](0, "div", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojection"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](2, "div", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojection"](3, 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]();
      }
    },
    styles: [".gradient[_ngcontent-%COMP%] {\n  position: relative;\n}\n.gradient[_ngcontent-%COMP%]:after {\n  content: \"\";\n  position: absolute;\n  left: 0;\n  top: 0;\n  width: 100%;\n  height: 100%;\n  display: inline-block;\n  background-image: var(--background-blend-gradient);\n}\n\n[_nghost-%COMP%] {\n  display: grid;\n  grid-template-columns: 40% 60%;\n  max-width: 120rem;\n  margin: 0 auto 7rem;\n}\n@media only screen and (max-width: 900px) {\n  [_nghost-%COMP%] {\n    display: block;\n    grid-template-columns: unset;\n    margin-bottom: 5rem;\n  }\n}\n@media only screen and (max-width: 1300px) {\n  [_nghost-%COMP%] {\n    max-width: 110rem;\n    margin-bottom: 5rem;\n  }\n  [_nghost-%COMP%]   .grid--item[_ngcontent-%COMP%] {\n    padding: 2rem;\n  }\n}\n@media only screen and (max-width: 1462.5px) {\n  [_nghost-%COMP%] {\n    max-width: 110rem;\n    margin-bottom: 6rem;\n  }\n}\n@media only screen and (max-width: 1500px) {\n  [_nghost-%COMP%] {\n    max-width: 105rem;\n  }\n}\n\n.grid--item[_ngcontent-%COMP%] {\n  padding: 4rem;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL3Rva2VuL19ncmFkaWVudC5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvY29tcG9uZW50L2RldGFpbC1ncmlkL2RldGFpbC1ncmlkLmNvbXBvbmVudC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBO0VBQ0Usa0JBQUE7QUNDRjtBRENFO0VBQ0UsV0FBQTtFQUNBLGtCQUFBO0VBQ0EsT0FBQTtFQUNBLE1BQUE7RUFDQSxXQUFBO0VBQ0EsWUFBQTtFQUNBLHFCQUFBO0VBQ0Esa0RBQUE7QUNDSjs7QUFWQTtFQUNFLGFBQUE7RUFDQSw4QkFBQTtFQUNBLGlCQUFBO0VBQ0EsbUJBQUE7QUFhRjtBQVhFO0VBTkY7SUFPSSxjQUFBO0lBQ0EsNEJBQUE7SUFDQSxtQkFBQTtFQWNGO0FBQ0Y7QUFaRTtFQVpGO0lBYUksaUJBQUE7SUFDQSxtQkFBQTtFQWVGO0VBYkk7SUFDRSxhQUFBO0VBZU47QUFDRjtBQVhFO0VBdEJGO0lBdUJJLGlCQUFBO0lBQ0EsbUJBQUE7RUFjRjtBQUNGO0FBWkU7RUEzQkY7SUE0QkksaUJBQUE7RUFlRjtBQUNGOztBQVhFO0VBQ0UsYUFBQTtBQWNKIiwic291cmNlc0NvbnRlbnQiOlsiLmdyYWRpZW50IHtcbiAgcG9zaXRpb246IHJlbGF0aXZlO1xuXG4gICY6YWZ0ZXIge1xuICAgIGNvbnRlbnQ6ICcnO1xuICAgIHBvc2l0aW9uOiBhYnNvbHV0ZTtcbiAgICBsZWZ0OiAwO1xuICAgIHRvcDogMDtcbiAgICB3aWR0aDogMTAwJTtcbiAgICBoZWlnaHQ6IDEwMCU7XG4gICAgZGlzcGxheTogaW5saW5lLWJsb2NrO1xuICAgIGJhY2tncm91bmQtaW1hZ2U6IHZhcigtLWJhY2tncm91bmQtYmxlbmQtZ3JhZGllbnQpO1xuICB9XG59XG4iLCJAaW1wb3J0ICcuLi8uLi90b2tlbi9ncmFkaWVudCc7XG5cbjpob3N0IHtcbiAgZGlzcGxheTogZ3JpZDtcbiAgZ3JpZC10ZW1wbGF0ZS1jb2x1bW5zOiA0MCUgNjAlO1xuICBtYXgtd2lkdGg6IDEyMHJlbTtcbiAgbWFyZ2luOiAwIGF1dG8gN3JlbTtcblxuICBAbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDkwMHB4KSB7XG4gICAgZGlzcGxheTogYmxvY2s7XG4gICAgZ3JpZC10ZW1wbGF0ZS1jb2x1bW5zOiB1bnNldDtcbiAgICBtYXJnaW4tYm90dG9tOiA1cmVtO1xuICB9XG5cbiAgQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiAxMzAwcHgpIHtcbiAgICBtYXgtd2lkdGg6IDExMHJlbTtcbiAgICBtYXJnaW4tYm90dG9tOiA1cmVtO1xuICAgIC5ncmlkIHtcbiAgICAgICYtLWl0ZW0ge1xuICAgICAgICBwYWRkaW5nOiAycmVtO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIEBtZWRpYSBvbmx5IHNjcmVlbiBhbmQgKG1heC13aWR0aDogMTQ2Mi41cHgpIHtcbiAgICBtYXgtd2lkdGg6IDExMHJlbTtcbiAgICBtYXJnaW4tYm90dG9tOiA2cmVtO1xuICB9XG5cbiAgQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiAxNTAwcHgpIHtcbiAgICBtYXgtd2lkdGg6IDEwNXJlbTtcbiAgfVxufVxuXG4uZ3JpZCB7XG4gICYtLWl0ZW0ge1xuICAgIHBhZGRpbmc6IDRyZW07XG4gIH1cbn1cbiJdLCJzb3VyY2VSb290IjoiIn0= */"],
    changeDetection: 0
  });
}


/***/ }),

/***/ 2541:
/*!********************************************************************!*\
  !*** ./node_modules/rxjs/dist/esm/internal/operators/takeWhile.js ***!
  \********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   takeWhile: () => (/* binding */ takeWhile)
/* harmony export */ });
/* harmony import */ var _util_lift__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../util/lift */ 4114);
/* harmony import */ var _OperatorSubscriber__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./OperatorSubscriber */ 5678);


function takeWhile(predicate, inclusive = false) {
  return (0,_util_lift__WEBPACK_IMPORTED_MODULE_0__.operate)((source, subscriber) => {
    let index = 0;
    source.subscribe((0,_OperatorSubscriber__WEBPACK_IMPORTED_MODULE_1__.createOperatorSubscriber)(subscriber, value => {
      const result = predicate(value, index++);
      (result || inclusive) && subscriber.next(value);
      !result && subscriber.complete();
    }));
  });
}

/***/ }),

/***/ 9668:
/*!********************************************************************!*\
  !*** ./node_modules/rxjs/dist/esm/internal/util/argsOrArgArray.js ***!
  \********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   argsOrArgArray: () => (/* binding */ argsOrArgArray)
/* harmony export */ });
const {
  isArray
} = Array;
function argsOrArgArray(args) {
  return args.length === 1 && isArray(args[0]) ? args[0] : args;
}

/***/ })

}]);
//# sourceMappingURL=common.js.map