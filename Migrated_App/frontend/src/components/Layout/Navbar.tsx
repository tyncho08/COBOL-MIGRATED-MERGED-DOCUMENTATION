'use client'

import { useState, useEffect, Fragment } from 'react'
import Link from 'next/link'
import { usePathname, useRouter } from 'next/navigation'
import { Menu, Transition } from '@headlessui/react'
import {
  BuildingOfficeIcon,
  UsersIcon,
  TruckIcon,
  CubeIcon,
  DocumentTextIcon,
  ChartBarIcon,
  BanknotesIcon,
  Cog6ToothIcon,
  HomeIcon,
  XMarkIcon,
  Bars3Icon,
  BellIcon,
  UserCircleIcon,
  ArrowRightOnRectangleIcon,
  MagnifyingGlassIcon
} from '@heroicons/react/24/outline'

const navigation = [
  { name: 'Dashboard', href: '/', icon: HomeIcon },
  { name: 'Sales Ledger', href: '/sales', icon: UsersIcon },
  { name: 'Purchase Ledger', href: '/purchase', icon: TruckIcon },
  { name: 'Stock Control', href: '/stock', icon: CubeIcon },
  { name: 'General Ledger', href: '/gl', icon: DocumentTextIcon },
  { name: 'Reports', href: '/reports', icon: ChartBarIcon },
  { name: 'Payments', href: '/payments', icon: BanknotesIcon },
  { name: 'Settings', href: '/settings', icon: Cog6ToothIcon },
]

interface NavbarProps {
  user?: {
    name: string
    email: string
    role: string
  }
}

export default function Navbar({ user }: NavbarProps) {
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false)
  const [currentUser, setCurrentUser] = useState(user)
  const [showNotifications, setShowNotifications] = useState(false)
  const [notifications, setNotifications] = useState<any[]>([])
  const [unreadCount, setUnreadCount] = useState(0)
  const pathname = usePathname()
  const router = useRouter()

  useEffect(() => {
    // Check localStorage for user data
    const storedUser = localStorage.getItem('user')
    if (storedUser) {
      setCurrentUser(JSON.parse(storedUser))
    }
    
    // Fetch notifications
    fetchNotifications()
  }, [])
  
  const fetchNotifications = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/notifications/unread')
      if (response.ok) {
        const data = await response.json()
        setNotifications(data.notifications || getMockNotifications())
        setUnreadCount(data.notifications?.filter((n: any) => !n.read).length || 2)
      } else {
        // Use mock data as fallback
        const mockNotifications = getMockNotifications()
        setNotifications(mockNotifications)
        setUnreadCount(mockNotifications.filter(n => !n.read).length)
      }
    } catch (error) {
      console.error('Failed to fetch notifications:', error)
      const mockNotifications = getMockNotifications()
      setNotifications(mockNotifications)
      setUnreadCount(mockNotifications.filter(n => !n.read).length)
    }
  }
  
  const getMockNotifications = () => [
    {
      id: 1,
      type: 'payment_received',
      title: 'Payment Received',
      message: 'Payment of $1,250.00 received from ABC Corporation',
      timestamp: '2024-01-15T10:30:00Z',
      read: false
    },
    {
      id: 2,
      type: 'stock_low',
      title: 'Low Stock Alert',
      message: 'Widget A - Blue is running low (25 units remaining)',
      timestamp: '2024-01-15T09:15:00Z',
      read: false
    }
  ]

  const handleLogout = () => {
    // Clear user session
    localStorage.removeItem('user')
    
    // Clear persistent session if exists (Remember Me)
    localStorage.removeItem('persistentSession')
    
    // Clear the cookie
    document.cookie = 'user=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;'
    
    // Redirect to login
    router.push('/login')
  }

  const displayUser = currentUser || {
    name: 'System Administrator',
    email: 'admin@acas.local',
    role: 'Administrator'
  }

  return (
    <>
      {/* Desktop Sidebar */}
      <div className="hidden lg:fixed lg:inset-y-0 lg:z-40 lg:flex lg:w-72 lg:flex-col">
        <div className="flex grow flex-col gap-y-5 overflow-y-auto bg-white border-r border-gray-200 px-6 pb-4">
          {/* Logo */}
          <div className="flex h-16 shrink-0 items-center">
            <BuildingOfficeIcon className="h-8 w-8 text-indigo-600" />
            <div className="ml-3">
              <h1 className="text-xl font-bold text-gray-900">ACAS</h1>
            </div>
          </div>

          {/* Navigation */}
          <nav className="flex flex-1 flex-col">
            <ul role="list" className="flex flex-1 flex-col gap-y-7">
              <li>
                <ul role="list" className="-mx-2 space-y-1">
                  {navigation.map((item) => {
                    const isActive = pathname === item.href
                    return (
                      <li key={item.name}>
                        <Link
                          href={item.href}
                          className={`
                            group flex gap-x-3 rounded-md p-2 text-sm leading-6 font-semibold transition-colors
                            ${isActive
                              ? 'bg-indigo-50 text-indigo-600'
                              : 'text-gray-700 hover:text-indigo-600 hover:bg-gray-50'
                            }
                          `}
                        >
                          <item.icon
                            className={`h-6 w-6 shrink-0 ${
                              isActive ? 'text-indigo-600' : 'text-gray-400 group-hover:text-indigo-600'
                            }`}
                          />
                          {item.name}
                        </Link>
                      </li>
                    )
                  })}
                </ul>
              </li>

              {/* System Status */}
              <li className="mt-auto">
                <div className="rounded-lg bg-gray-50 p-4">
                  <h3 className="text-sm font-medium text-gray-900">System Status</h3>
                  <div className="mt-2 space-y-2">
                    <div className="flex items-center justify-between text-xs">
                      <span className="text-gray-500">Database:</span>
                      <div className="flex items-center">
                        <div className="w-2 h-2 bg-green-500 rounded-full mr-1" />
                        <span className="text-gray-900">Connected</span>
                      </div>
                    </div>
                    <div className="flex items-center justify-between text-xs">
                      <span className="text-gray-500">GL Period:</span>
                      <span className="text-gray-900">Open</span>
                    </div>
                    <div className="flex items-center justify-between text-xs">
                      <span className="text-gray-500">Balance:</span>
                      <span className="text-green-600 font-medium">Balanced</span>
                    </div>
                  </div>
                </div>
              </li>
            </ul>
          </nav>
        </div>
      </div>

      {/* Mobile menu */}
      <div className="lg:hidden">
        {/* Mobile menu button */}
        <div className="sticky top-0 z-40 flex h-16 shrink-0 items-center gap-x-4 border-b border-gray-200 bg-white px-4 shadow-sm sm:gap-x-6 sm:px-6">
          <button
            type="button"
            className="-m-2.5 p-2.5 text-gray-700 lg:hidden"
            onClick={() => setMobileMenuOpen(true)}
          >
            <span className="sr-only">Open sidebar</span>
            <Bars3Icon className="h-6 w-6" />
          </button>

          {/* Separator */}
          <div className="h-6 w-px bg-gray-200 lg:hidden" />

          <div className="flex flex-1 gap-x-4 self-stretch lg:gap-x-6">
            <div className="flex items-center gap-x-4 lg:gap-x-6">
              {/* Logo for mobile */}
              <div className="flex items-center">
                <BuildingOfficeIcon className="h-6 w-6 text-indigo-600" />
                <span className="ml-2 text-lg font-bold text-gray-900">ACAS</span>
              </div>
            </div>
            <div className="flex items-center gap-x-4 lg:gap-x-6 ml-auto">
              {/* Search Button */}
              <button
                onClick={() => router.push('/search')}
                className="-m-2.5 p-2.5 text-gray-400 hover:text-gray-500"
              >
                <span className="sr-only">Search</span>
                <MagnifyingGlassIcon className="h-5 w-5" />
              </button>
              
              {/* Notifications dropdown */}
              <Menu as="div" className="relative">
                <Menu.Button className="relative -m-2.5 p-2.5 text-gray-400 hover:text-gray-500">
                  <span className="sr-only">View notifications</span>
                  <BellIcon className="h-6 w-6" />
                  {unreadCount > 0 && (
                    <span className="absolute top-0 right-0 inline-flex items-center justify-center h-5 w-5 text-xs font-bold text-white bg-red-500 rounded-full">
                      {unreadCount}
                    </span>
                  )}
                </Menu.Button>
                <Transition
                  as={Fragment}
                  enter="transition ease-out duration-100"
                  enterFrom="transform opacity-0 scale-95"
                  enterTo="transform opacity-100 scale-100"
                  leave="transition ease-in duration-75"
                  leaveFrom="transform opacity-100 scale-100"
                  leaveTo="transform opacity-0 scale-95"
                >
                  <Menu.Items className="absolute right-0 z-10 mt-2 w-80 origin-top-right rounded-md bg-white py-1 shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none">
                    <div className="px-4 py-2 border-b border-gray-200">
                      <h3 className="text-sm font-semibold text-gray-900">Notifications</h3>
                    </div>
                    <div className="max-h-96 overflow-y-auto">
                      {notifications.length > 0 ? (
                        notifications.map(notification => (
                          <Menu.Item key={notification.id}>
                            {({ active }) => (
                              <div className={`${active ? 'bg-gray-50' : ''} px-4 py-3 ${!notification.read ? 'bg-blue-50' : ''}`}>
                                <p className="text-sm font-medium text-gray-900">{notification.title}</p>
                                <p className="text-sm text-gray-500 mt-1">{notification.message}</p>
                                <p className="text-xs text-gray-400 mt-1">
                                  {new Date(notification.timestamp).toLocaleTimeString()}
                                </p>
                              </div>
                            )}
                          </Menu.Item>
                        ))
                      ) : (
                        <div className="px-4 py-8 text-center">
                          <BellIcon className="mx-auto h-12 w-12 text-gray-400" />
                          <p className="mt-2 text-sm text-gray-500">No new notifications</p>
                        </div>
                      )}
                    </div>
                    <div className="px-4 py-2 border-t border-gray-200">
                      <Link 
                        href="/settings?tab=Notifications"
                        className="text-sm text-indigo-600 hover:text-indigo-500 font-medium"
                      >
                        View all notifications →
                      </Link>
                    </div>
                  </Menu.Items>
                </Transition>
              </Menu>

              {/* Separator */}
              <div className="hidden lg:block lg:h-6 lg:w-px lg:bg-gray-200" />

              {/* Profile dropdown */}
              <Menu as="div" className="relative">
                <Menu.Button className="-m-1.5 flex items-center p-1.5">
                  <span className="sr-only">Open user menu</span>
                  <UserCircleIcon className="h-8 w-8 text-gray-400" />
                  <span className="hidden lg:flex lg:items-center">
                    <span className="ml-4 text-sm font-semibold leading-6 text-gray-900">
                      {displayUser.name}
                    </span>
                  </span>
                </Menu.Button>
                <Transition
                  as={Fragment}
                  enter="transition ease-out duration-100"
                  enterFrom="transform opacity-0 scale-95"
                  enterTo="transform opacity-100 scale-100"
                  leave="transition ease-in duration-75"
                  leaveFrom="transform opacity-100 scale-100"
                  leaveTo="transform opacity-0 scale-95"
                >
                  <Menu.Items className="absolute right-0 z-10 mt-2 w-48 origin-top-right rounded-md bg-white py-1 shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none">
                    <Menu.Item>
                      {({ active }) => (
                        <button
                          onClick={handleLogout}
                          className={`${active ? 'bg-gray-100' : ''} flex w-full px-4 py-2 text-sm text-gray-700`}
                        >
                          <ArrowRightOnRectangleIcon className="mr-3 h-5 w-5 text-gray-400" />
                          Sign out
                        </button>
                      )}
                    </Menu.Item>
                  </Menu.Items>
                </Transition>
              </Menu>
            </div>
          </div>
        </div>

        {/* Mobile menu overlay */}
        {mobileMenuOpen && (
          <div className="relative z-50 lg:hidden">
            <div className="fixed inset-0 bg-gray-900/80" />
            <div className="fixed inset-0 flex">
              <div className="relative mr-16 flex w-full max-w-xs flex-1">
                <div className="absolute left-full top-0 flex w-16 justify-center pt-5">
                  <button
                    type="button"
                    className="-m-2.5 p-2.5"
                    onClick={() => setMobileMenuOpen(false)}
                  >
                    <span className="sr-only">Close sidebar</span>
                    <XMarkIcon className="h-6 w-6 text-white" />
                  </button>
                </div>

                <div className="flex grow flex-col gap-y-5 overflow-y-auto bg-white px-6 pb-4">
                  <div className="flex h-16 shrink-0 items-center">
                    <BuildingOfficeIcon className="h-8 w-8 text-indigo-600" />
                    <div className="ml-3">
                      <h1 className="text-xl font-bold text-gray-900">ACAS</h1>
                    </div>
                  </div>
                  <nav className="flex flex-1 flex-col">
                    <ul role="list" className="flex flex-1 flex-col gap-y-7">
                      <li>
                        <ul role="list" className="-mx-2 space-y-1">
                          {navigation.map((item) => {
                            const isActive = pathname === item.href
                            return (
                              <li key={item.name}>
                                <Link
                                  href={item.href}
                                  onClick={() => setMobileMenuOpen(false)}
                                  className={`
                                    group flex gap-x-3 rounded-md p-2 text-sm leading-6 font-semibold
                                    ${isActive
                                      ? 'bg-indigo-50 text-indigo-600'
                                      : 'text-gray-700 hover:text-indigo-600 hover:bg-gray-50'
                                    }
                                  `}
                                >
                                  <item.icon
                                    className={`h-6 w-6 shrink-0 ${
                                      isActive ? 'text-indigo-600' : 'text-gray-400 group-hover:text-indigo-600'
                                    }`}
                                  />
                                  {item.name}
                                </Link>
                              </li>
                            )
                          })}
                        </ul>
                      </li>
                    </ul>
                  </nav>
                </div>
              </div>
            </div>
          </div>
        )}
      </div>

      {/* Desktop top bar */}
      <div className="hidden lg:block lg:pl-72">
        <div className="sticky top-0 z-40 flex h-16 shrink-0 items-center gap-x-4 border-b border-gray-200 bg-white px-4 shadow-sm sm:gap-x-6 sm:px-6">
          <div className="flex flex-1 gap-x-4 self-stretch lg:gap-x-6">
            <div className="relative flex flex-1">
              {/* Breadcrumb could go here */}
            </div>
            <div className="flex items-center gap-x-4 lg:gap-x-6">
              {/* Search Button */}
              <button
                onClick={() => router.push('/search')}
                className="-m-2.5 p-2.5 text-gray-400 hover:text-gray-500"
              >
                <span className="sr-only">Search</span>
                <MagnifyingGlassIcon className="h-6 w-6" />
              </button>
              
              {/* Notifications dropdown */}
              <Menu as="div" className="relative">
                <Menu.Button className="relative -m-2.5 p-2.5 text-gray-400 hover:text-gray-500">
                  <span className="sr-only">View notifications</span>
                  <BellIcon className="h-6 w-6" />
                  {unreadCount > 0 && (
                    <span className="absolute top-0 right-0 inline-flex items-center justify-center h-5 w-5 text-xs font-bold text-white bg-red-500 rounded-full">
                      {unreadCount}
                    </span>
                  )}
                </Menu.Button>
                <Transition
                  as={Fragment}
                  enter="transition ease-out duration-100"
                  enterFrom="transform opacity-0 scale-95"
                  enterTo="transform opacity-100 scale-100"
                  leave="transition ease-in duration-75"
                  leaveFrom="transform opacity-100 scale-100"
                  leaveTo="transform opacity-0 scale-95"
                >
                  <Menu.Items className="absolute right-0 z-10 mt-2 w-80 origin-top-right rounded-md bg-white py-1 shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none">
                    <div className="px-4 py-2 border-b border-gray-200">
                      <h3 className="text-sm font-semibold text-gray-900">Notifications</h3>
                    </div>
                    <div className="max-h-96 overflow-y-auto">
                      {notifications.length > 0 ? (
                        notifications.map(notification => (
                          <Menu.Item key={notification.id}>
                            {({ active }) => (
                              <div className={`${active ? 'bg-gray-50' : ''} px-4 py-3 ${!notification.read ? 'bg-blue-50' : ''}`}>
                                <p className="text-sm font-medium text-gray-900">{notification.title}</p>
                                <p className="text-sm text-gray-500 mt-1">{notification.message}</p>
                                <p className="text-xs text-gray-400 mt-1">
                                  {new Date(notification.timestamp).toLocaleTimeString()}
                                </p>
                              </div>
                            )}
                          </Menu.Item>
                        ))
                      ) : (
                        <div className="px-4 py-8 text-center">
                          <BellIcon className="mx-auto h-12 w-12 text-gray-400" />
                          <p className="mt-2 text-sm text-gray-500">No new notifications</p>
                        </div>
                      )}
                    </div>
                    <div className="px-4 py-2 border-t border-gray-200">
                      <Link 
                        href="/settings?tab=Notifications"
                        className="text-sm text-indigo-600 hover:text-indigo-500 font-medium"
                      >
                        View all notifications →
                      </Link>
                    </div>
                  </Menu.Items>
                </Transition>
              </Menu>

              {/* Separator */}
              <div className="hidden lg:block lg:h-6 lg:w-px lg:bg-gray-200" />

              {/* Profile dropdown */}
              <Menu as="div" className="relative">
                <Menu.Button className="-m-1.5 flex items-center p-1.5">
                  <span className="sr-only">Open user menu</span>
                  <UserCircleIcon className="h-8 w-8 text-gray-400" />
                  <span className="hidden lg:flex lg:items-center">
                    <span className="ml-4 text-sm font-semibold leading-6 text-gray-900">
                      {displayUser.name}
                    </span>
                    <span className="ml-2 text-xs text-gray-500">({displayUser.role})</span>
                  </span>
                </Menu.Button>
                <Transition
                  as={Fragment}
                  enter="transition ease-out duration-100"
                  enterFrom="transform opacity-0 scale-95"
                  enterTo="transform opacity-100 scale-100"
                  leave="transition ease-in duration-75"
                  leaveFrom="transform opacity-100 scale-100"
                  leaveTo="transform opacity-0 scale-95"
                >
                  <Menu.Items className="absolute right-0 z-10 mt-2 w-48 origin-top-right rounded-md bg-white py-1 shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none">
                    <Menu.Item>
                      {({ active }) => (
                        <button
                          onClick={handleLogout}
                          className={`${active ? 'bg-gray-100' : ''} flex w-full px-4 py-2 text-sm text-gray-700`}
                        >
                          <ArrowRightOnRectangleIcon className="mr-3 h-5 w-5 text-gray-400" />
                          Sign out
                        </button>
                      )}
                    </Menu.Item>
                  </Menu.Items>
                </Transition>
              </Menu>
            </div>
          </div>
        </div>
      </div>
    </>
  )
}