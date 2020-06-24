import React, {useState, useCallback} from 'react';
import classnames from 'classnames';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useLockBodyScroll from '@theme/hooks/useLockBodyScroll';
import useLogo from '@theme/hooks/useLogo';
import Link from '@docusaurus/Link';
import isInternalUrl from '@docusaurus/isInternalUrl';

import styles from './styles.module.css';

const MOBILE_TOGGLE_SIZE = 24;

function DocSidebarItem({
  item,
  onItemClick,
  collapsible,
  activePath,
  ...props
}) {
  const {items, href, label, type} = item;
  const [collapsed, setCollapsed] = useState(item.collapsed);
  const [prevCollapsedProp, setPreviousCollapsedProp] = useState(null);

  // If the collapsing state from props changed, probably a navigation event
  // occurred. Overwrite the component's collapsed state with the props'
  // collapsed value.
  if (item.collapsed !== prevCollapsedProp) {
    setPreviousCollapsedProp(item.collapsed);
    setCollapsed(item.collapsed);
  }

  const handleItemClick = useCallback((e) => {
    e.preventDefault();
    e.target.blur();
    setCollapsed((state) => !state);
  });

  switch (type) {
    case 'category':
      return (
        items.length > 0 && (
          <li
            className={classnames('menu__list-item', {
              'menu__list-item--collapsed': collapsed,
            })}
            key={label}>
            <a
              className={classnames('menu__link', {
                'menu__link--sublist': collapsible,
                'menu__link--active': collapsible && !item.collapsed,
              })}
              href="#!"
              onClick={collapsible ? handleItemClick : undefined}
              {...props}>
              {label}
            </a>
            <ul className="menu__list">
              {items.map((childItem) => (
                <DocSidebarItem
                  tabIndex={collapsed ? '-1' : '0'}
                  key={childItem.label}
                  item={childItem}
                  onItemClick={onItemClick}
                  collapsible={collapsible}
                  activePath={activePath}
                />
              ))}
            </ul>
          </li>
        )
      );

    case 'link':
    default:
      return (
        <li className="menu__list-item" key={label}>
          <Link
            className={classnames('menu__link', {
              'menu__link--active': href === activePath,
            })}
            to={href}
            {...(isInternalUrl(href)
              ? {
                  isNavLink: true,
                  exact: true,
                  onClick: onItemClick,
                }
              : {
                  target: '_blank',
                  rel: 'noreferrer noopener',
                })}
            {...props}>
            {label}
          </Link>
        </li>
      );
  }
}

// Calculate the category collapsing state when a page navigation occurs.
// We want to automatically expand the categories which contains the current page.
function mutateSidebarCollapsingState(item, path) {
  const {items, href, type} = item;
  switch (type) {
    case 'category': {
      const anyChildItemsActive =
        items
          .map((childItem) => mutateSidebarCollapsingState(childItem, path))
          .filter((val) => val).length > 0;
      // eslint-disable-next-line no-param-reassign
      item.collapsed = !anyChildItemsActive;
      return anyChildItemsActive;
    }

    case 'link':
    default:
      return href === path;
  }
}

function DocSidebar(props) {
  const [showResponsiveSidebar, setShowResponsiveSidebar] = useState(false);
  const {
    siteConfig: {
      themeConfig: {navbar: {title, hideOnScroll = false} = {}},
    } = {},
    isClient,
  } = useDocusaurusContext();
  const {logoLink, logoLinkProps, logoImageUrl, logoAlt} = useLogo();

  const {
    docsSidebars,
    path,
    sidebar: currentSidebar,
    sidebarCollapsible,
  } = props;

  useLockBodyScroll(showResponsiveSidebar);

  if (!currentSidebar) {
    return null;
  }

  const sidebarData = docsSidebars[currentSidebar];

  if (!sidebarData) {
    throw new Error(
      `Cannot find the sidebar "${currentSidebar}" in the sidebar config!`,
    );
  }

  if (sidebarCollapsible) {
    sidebarData.forEach((sidebarItem) =>
      mutateSidebarCollapsingState(sidebarItem, path),
    );
  }

  return (
    <div className={styles.sidebar}>
      {hideOnScroll && (
        <Link
          tabIndex="-1"
          className={styles.sidebarLogo}
          to={logoLink}
          {...logoLinkProps}>
          {logoImageUrl != null && (
            <img key={isClient} src={logoImageUrl} alt={logoAlt} />
          )}
          {title != null && <strong>{title}</strong>}
        </Link>
      )}
      <div
        className={classnames('menu', styles.menu)}>
        <ul className="menu__list">
          {sidebarData.map((item) => (
            <DocSidebarItem
              key={item.label}
              item={item}
              onItemClick={(e) => {
                e.target.blur();
                setShowResponsiveSidebar(false);
              }}
              collapsible={sidebarCollapsible}
              activePath={path}
            />
          ))}
        </ul>
      </div>
    </div>
  );
}

export default DocSidebar;
