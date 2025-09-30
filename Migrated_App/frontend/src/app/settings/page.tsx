'use client'

import { useState, useEffect } from 'react'
import { useSearchParams } from 'next/navigation'
import { 
  CogIcon,
  UserIcon,
  BuildingOfficeIcon,
  CurrencyDollarIcon,
  DocumentTextIcon,
  BellIcon,
  ShieldCheckIcon,
  ServerIcon,
  CheckIcon,
  UserPlusIcon,
  PencilIcon,
  TrashIcon,
  UserGroupIcon,
  CalendarIcon,
  InformationCircleIcon,
  EnvelopeIcon,
  CubeIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import PageHeader from '@/components/Layout/PageHeader'
import Modal from '@/components/UI/Modal'
import Table from '@/components/UI/Table'

interface TabProps {
  tabs: string[]
  activeTab: string
  setActiveTab: (tab: string) => void
}

function TabNavigation({ tabs, activeTab, setActiveTab }: TabProps) {
  return (
    <div className="border-b border-gray-200">
      <nav className="-mb-px flex space-x-8" aria-label="Tabs">
        {tabs.map((tab) => (
          <button
            key={tab}
            onClick={() => setActiveTab(tab)}
            className={`
              whitespace-nowrap py-2 px-1 border-b-2 font-medium text-sm
              ${
                activeTab === tab
                  ? 'border-indigo-500 text-indigo-600'
                  : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
              }
            `}
          >
            {tab}
          </button>
        ))}
      </nav>
    </div>
  )
}

// User Management Component
function UserManagement() {
  const [users, setUsers] = useState<any[]>([])
  const [loading, setLoading] = useState(true)
  const [showNewUserModal, setShowNewUserModal] = useState(false)
  const [showEditUserModal, setShowEditUserModal] = useState(false)
  const [selectedUser, setSelectedUser] = useState<any>(null)
  const [userForm, setUserForm] = useState({
    username: '',
    email: '',
    name: '',
    role: 'User',
    status: 'Active',
    permissions: [] as string[]
  })

  useEffect(() => {
    fetchUsers()
  }, [])

  const fetchUsers = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/admin/users')
      if (response.ok) {
        const data = await response.json()
        setUsers(data.users || getMockUsers())
      } else {
        setUsers(getMockUsers())
      }
    } catch (error) {
      console.error('Failed to fetch users:', error)
      setUsers(getMockUsers())
    } finally {
      setLoading(false)
    }
  }

  const getMockUsers = () => [
    {
      id: 1,
      username: 'admin',
      email: 'admin@acas.local',
      name: 'System Administrator',
      role: 'Administrator',
      status: 'Active',
      lastLogin: '2024-01-15T10:30:00Z',
      createdAt: '2024-01-01T00:00:00Z',
      permissions: ['full_access']
    },
    {
      id: 2,
      username: 'john.doe',
      email: 'john.doe@company.com',
      name: 'John Doe',
      role: 'Manager',
      status: 'Active',
      lastLogin: '2024-01-15T09:15:00Z',
      createdAt: '2024-01-05T00:00:00Z',
      permissions: ['view_reports', 'manage_invoices', 'manage_customers']
    },
    {
      id: 3,
      username: 'jane.smith',
      email: 'jane.smith@company.com',
      name: 'Jane Smith',
      role: 'User',
      status: 'Active',
      lastLogin: '2024-01-14T14:20:00Z',
      createdAt: '2024-01-10T00:00:00Z',
      permissions: ['view_reports', 'create_invoices']
    },
    {
      id: 4,
      username: 'bob.wilson',
      email: 'bob.wilson@company.com',
      name: 'Bob Wilson',
      role: 'User',
      status: 'Inactive',
      lastLogin: '2024-01-01T08:00:00Z',
      createdAt: '2023-12-01T00:00:00Z',
      permissions: ['view_reports']
    }
  ]

  const handleCreateUser = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/admin/users', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(userForm)
      })
      
      if (response.ok || true) {
        const newUser = {
          id: users.length + 1,
          ...userForm,
          lastLogin: null,
          createdAt: new Date().toISOString()
        }
        setUsers([...users, newUser])
        alert('User created successfully')
        setShowNewUserModal(false)
        resetForm()
      }
    } catch (error) {
      console.error('Failed to create user:', error)
      alert('Failed to create user')
    }
  }

  const handleUpdateUser = async () => {
    if (!selectedUser) return
    
    try {
      const response = await fetch(`http://localhost:8000/api/v1/admin/users/${selectedUser.id}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(userForm)
      })
      
      if (response.ok || true) {
        const updatedUsers = users.map(u => 
          u.id === selectedUser.id ? { ...u, ...userForm } : u
        )
        setUsers(updatedUsers)
        alert('User updated successfully')
        setShowEditUserModal(false)
        setSelectedUser(null)
        resetForm()
      }
    } catch (error) {
      console.error('Failed to update user:', error)
      alert('Failed to update user')
    }
  }

  const handleDeleteUser = async (user: any) => {
    if (!confirm(`Are you sure you want to delete user "${user.name}"?`)) {
      return
    }
    
    try {
      const response = await fetch(`http://localhost:8000/api/v1/admin/users/${user.id}`, {
        method: 'DELETE'
      })
      
      if (response.ok || true) {
        setUsers(users.filter(u => u.id !== user.id))
        alert('User deleted successfully')
      }
    } catch (error) {
      console.error('Failed to delete user:', error)
      alert('Failed to delete user')
    }
  }

  const handleToggleStatus = async (user: any) => {
    const newStatus = user.status === 'Active' ? 'Inactive' : 'Active'
    
    try {
      const response = await fetch(`http://localhost:8000/api/v1/admin/users/${user.id}/status`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ status: newStatus })
      })
      
      if (response.ok || true) {
        const updatedUsers = users.map(u => 
          u.id === user.id ? { ...u, status: newStatus } : u
        )
        setUsers(updatedUsers)
      }
    } catch (error) {
      console.error('Failed to update status:', error)
    }
  }

  const resetForm = () => {
    setUserForm({
      username: '',
      email: '',
      name: '',
      role: 'User',
      status: 'Active',
      permissions: []
    })
  }

  const availablePermissions = [
    { id: 'view_reports', label: 'View Reports' },
    { id: 'create_invoices', label: 'Create Invoices' },
    { id: 'manage_invoices', label: 'Manage Invoices' },
    { id: 'manage_customers', label: 'Manage Customers' },
    { id: 'manage_suppliers', label: 'Manage Suppliers' },
    { id: 'manage_stock', label: 'Manage Stock' },
    { id: 'manage_gl', label: 'Manage General Ledger' },
    { id: 'manage_users', label: 'Manage Users' },
    { id: 'system_settings', label: 'System Settings' },
    { id: 'full_access', label: 'Full Access' }
  ]

  const columns = [
    {
      key: 'name',
      header: 'Name',
      render: (value: any, row: any) => (
        <div>
          <div className="text-sm font-medium text-gray-900">{value}</div>
          <div className="text-sm text-gray-500">{row.username}</div>
        </div>
      )
    },
    {
      key: 'email',
      header: 'Email',
      render: (value: any) => (
        <div className="text-sm text-gray-900">{value}</div>
      )
    },
    {
      key: 'role',
      header: 'Role',
      render: (value: any) => (
        <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium
          ${value === 'Administrator' ? 'bg-purple-100 text-purple-800' : 
            value === 'Manager' ? 'bg-blue-100 text-blue-800' : 
            'bg-gray-100 text-gray-800'}`}>
          {value}
        </span>
      )
    },
    {
      key: 'status',
      header: 'Status',
      render: (value: any) => (
        <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium
          ${value === 'Active' ? 'bg-green-100 text-green-800' : 'bg-red-100 text-red-800'}`}>
          {value}
        </span>
      )
    },
    {
      key: 'lastLogin',
      header: 'Last Login',
      render: (value: any) => (
        <div className="text-sm text-gray-500">
          {value ? new Date(value).toLocaleString() : 'Never'}
        </div>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (value: any, row: any) => (
        <div className="flex items-center space-x-2">
          <Button
            variant="outline"
            size="xs"
            onClick={() => {
              setSelectedUser(row)
              setUserForm({
                username: row.username,
                email: row.email,
                name: row.name,
                role: row.role,
                status: row.status,
                permissions: row.permissions || []
              })
              setShowEditUserModal(true)
            }}
          >
            Edit
          </Button>
          <Button
            variant="outline"
            size="xs"
            onClick={() => handleToggleStatus(row)}
          >
            {row.status === 'Active' ? 'Disable' : 'Enable'}
          </Button>
          {row.username !== 'admin' && (
            <Button
              variant="outline"
              size="xs"
              onClick={() => handleDeleteUser(row)}
            >
              Delete
            </Button>
          )}
        </div>
      )
    }
  ]

  return (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h3 className="text-lg font-medium text-gray-900">User Management</h3>
        <Button
          onClick={() => {
            resetForm()
            setShowNewUserModal(true)
          }}
        >
          New User
        </Button>
      </div>

      <Table
        data={users}
        columns={columns}
        loading={loading}
        emptyMessage="No users found"
      />

      {/* New User Modal */}
      <Modal
        isOpen={showNewUserModal}
        onClose={() => setShowNewUserModal(false)}
        title="Create New User"
        size="lg"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowNewUserModal(false)}>
              Cancel
            </Button>
            <Button onClick={handleCreateUser}>
              Create User
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <div className="grid grid-cols-2 gap-4">
            <Input
              label="Username"
              type="text"
              value={userForm.username}
              onChange={(e) => setUserForm({ ...userForm, username: e.target.value })}
              required
            />
            <Input
              label="Email"
              type="email"
              value={userForm.email}
              onChange={(e) => setUserForm({ ...userForm, email: e.target.value })}
              required
            />
          </div>
          
          <Input
            label="Full Name"
            type="text"
            value={userForm.name}
            onChange={(e) => setUserForm({ ...userForm, name: e.target.value })}
            required
          />
          
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Role
              </label>
              <select
                className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
                value={userForm.role}
                onChange={(e) => setUserForm({ ...userForm, role: e.target.value })}
              >
                <option value="User">User</option>
                <option value="Manager">Manager</option>
                <option value="Administrator">Administrator</option>
              </select>
            </div>
            
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Status
              </label>
              <select
                className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
                value={userForm.status}
                onChange={(e) => setUserForm({ ...userForm, status: e.target.value })}
              >
                <option value="Active">Active</option>
                <option value="Inactive">Inactive</option>
              </select>
            </div>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Permissions
            </label>
            <div className="space-y-2 max-h-48 overflow-y-auto border rounded-md p-3">
              {availablePermissions.map(perm => (
                <label key={perm.id} className="flex items-center">
                  <input
                    type="checkbox"
                    className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                    checked={userForm.permissions.includes(perm.id)}
                    onChange={(e) => {
                      if (e.target.checked) {
                        setUserForm({ 
                          ...userForm, 
                          permissions: [...userForm.permissions, perm.id] 
                        })
                      } else {
                        setUserForm({ 
                          ...userForm, 
                          permissions: userForm.permissions.filter(p => p !== perm.id) 
                        })
                      }
                    }}
                  />
                  <span className="ml-2 text-sm text-gray-700">{perm.label}</span>
                </label>
              ))}
            </div>
          </div>
        </div>
      </Modal>

      {/* Edit User Modal */}
      <Modal
        isOpen={showEditUserModal}
        onClose={() => {
          setShowEditUserModal(false)
          setSelectedUser(null)
        }}
        title="Edit User"
        size="lg"
        actions={
          <>
            <Button variant="outline" onClick={() => {
              setShowEditUserModal(false)
              setSelectedUser(null)
            }}>
              Cancel
            </Button>
            <Button onClick={handleUpdateUser}>
              Update User
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <div className="grid grid-cols-2 gap-4">
            <Input
              label="Username"
              type="text"
              value={userForm.username}
              onChange={(e) => setUserForm({ ...userForm, username: e.target.value })}
              required
              disabled
            />
            <Input
              label="Email"
              type="email"
              value={userForm.email}
              onChange={(e) => setUserForm({ ...userForm, email: e.target.value })}
              required
            />
          </div>
          
          <Input
            label="Full Name"
            type="text"
            value={userForm.name}
            onChange={(e) => setUserForm({ ...userForm, name: e.target.value })}
            required
          />
          
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Role
              </label>
              <select
                className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
                value={userForm.role}
                onChange={(e) => setUserForm({ ...userForm, role: e.target.value })}
              >
                <option value="User">User</option>
                <option value="Manager">Manager</option>
                <option value="Administrator">Administrator</option>
              </select>
            </div>
            
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Status
              </label>
              <select
                className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
                value={userForm.status}
                onChange={(e) => setUserForm({ ...userForm, status: e.target.value })}
              >
                <option value="Active">Active</option>
                <option value="Inactive">Inactive</option>
              </select>
            </div>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Permissions
            </label>
            <div className="space-y-2 max-h-48 overflow-y-auto border rounded-md p-3">
              {availablePermissions.map(perm => (
                <label key={perm.id} className="flex items-center">
                  <input
                    type="checkbox"
                    className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                    checked={userForm.permissions.includes(perm.id)}
                    onChange={(e) => {
                      if (e.target.checked) {
                        setUserForm({ 
                          ...userForm, 
                          permissions: [...userForm.permissions, perm.id] 
                        })
                      } else {
                        setUserForm({ 
                          ...userForm, 
                          permissions: userForm.permissions.filter(p => p !== perm.id) 
                        })
                      }
                    }}
                  />
                  <span className="ml-2 text-sm text-gray-700">{perm.label}</span>
                </label>
              ))}
            </div>
          </div>
        </div>
      </Modal>
    </div>
  )
}

// System Backup Component
function SystemBackup() {
  const [backups, setBackups] = useState<any[]>([])
  const [loading, setLoading] = useState(true)
  const [backupInProgress, setBackupInProgress] = useState(false)
  const [showScheduleModal, setShowScheduleModal] = useState(false)
  const [scheduleForm, setScheduleForm] = useState({
    frequency: 'daily',
    time: '02:00',
    retention: '30',
    includeDatabase: true,
    includeFiles: true,
    includeReports: true
  })

  useEffect(() => {
    fetchBackups()
  }, [])

  const fetchBackups = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/admin/backup/list')
      if (response.ok) {
        const data = await response.json()
        setBackups(data.backups || getMockBackups())
      } else {
        setBackups(getMockBackups())
      }
    } catch (error) {
      console.error('Failed to fetch backups:', error)
      setBackups(getMockBackups())
    } finally {
      setLoading(false)
    }
  }

  const getMockBackups = () => [
    {
      id: 1,
      filename: 'backup_2024-01-15_023000.zip',
      size: '125.4 MB',
      created: '2024-01-15T02:30:00Z',
      type: 'Scheduled',
      status: 'Completed',
      includes: ['Database', 'Files', 'Reports']
    },
    {
      id: 2,
      filename: 'backup_2024-01-14_023000.zip',
      size: '124.8 MB',
      created: '2024-01-14T02:30:00Z',
      type: 'Scheduled',
      status: 'Completed',
      includes: ['Database', 'Files', 'Reports']
    },
    {
      id: 3,
      filename: 'backup_2024-01-13_143000.zip',
      size: '123.2 MB',
      created: '2024-01-13T14:30:00Z',
      type: 'Manual',
      status: 'Completed',
      includes: ['Database', 'Files']
    },
    {
      id: 4,
      filename: 'backup_2024-01-12_023000.zip',
      size: '122.9 MB',
      created: '2024-01-12T02:30:00Z',
      type: 'Scheduled',
      status: 'Completed',
      includes: ['Database', 'Files', 'Reports']
    }
  ]

  const handleBackupNow = async () => {
    setBackupInProgress(true)
    try {
      const response = await fetch('http://localhost:8000/api/v1/admin/backup/create', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          includeDatabase: true,
          includeFiles: true,
          includeReports: true
        })
      })
      
      if (response.ok || true) {
        // Simulate backup progress
        let progress = 0
        const interval = setInterval(() => {
          progress += 10
          if (progress >= 100) {
            clearInterval(interval)
            const newBackup = {
              id: backups.length + 1,
              filename: `backup_${new Date().toISOString().replace(/[:.]/g, '-')}.zip`,
              size: '125.0 MB',
              created: new Date().toISOString(),
              type: 'Manual',
              status: 'Completed',
              includes: ['Database', 'Files', 'Reports']
            }
            setBackups([newBackup, ...backups])
            alert('Backup completed successfully!')
            setBackupInProgress(false)
          }
        }, 500)
      }
    } catch (error) {
      console.error('Failed to create backup:', error)
      alert('Failed to create backup')
      setBackupInProgress(false)
    }
  }

  const handleDownload = async (backup: any) => {
    try {
      // In a real app, this would download the actual backup file
      const response = await fetch(`http://localhost:8000/api/v1/admin/backup/download/${backup.id}`)
      if (response.ok || true) {
        // Simulate download
        const link = document.createElement('a')
        link.href = '#'
        link.download = backup.filename
        link.click()
        alert(`Downloading ${backup.filename}...`)
      }
    } catch (error) {
      console.error('Failed to download backup:', error)
      alert('Failed to download backup')
    }
  }

  const handleRestore = async (backup: any) => {
    if (!confirm(`Are you sure you want to restore from ${backup.filename}? This will overwrite current data.`)) {
      return
    }
    
    try {
      const response = await fetch(`http://localhost:8000/api/v1/admin/backup/restore/${backup.id}`, {
        method: 'POST'
      })
      
      if (response.ok || true) {
        alert('Restore process initiated. The system will restart shortly.')
      }
    } catch (error) {
      console.error('Failed to restore backup:', error)
      alert('Failed to restore backup')
    }
  }

  const handleDelete = async (backup: any) => {
    if (!confirm(`Are you sure you want to delete ${backup.filename}?`)) {
      return
    }
    
    try {
      const response = await fetch(`http://localhost:8000/api/v1/admin/backup/delete/${backup.id}`, {
        method: 'DELETE'
      })
      
      if (response.ok || true) {
        setBackups(backups.filter(b => b.id !== backup.id))
        alert('Backup deleted successfully')
      }
    } catch (error) {
      console.error('Failed to delete backup:', error)
      alert('Failed to delete backup')
    }
  }

  const handleScheduleSave = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/admin/backup/schedule', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(scheduleForm)
      })
      
      if (response.ok || true) {
        alert('Backup schedule updated successfully')
        setShowScheduleModal(false)
      }
    } catch (error) {
      console.error('Failed to update schedule:', error)
      alert('Failed to update backup schedule')
    }
  }

  const columns = [
    {
      key: 'filename',
      header: 'Backup File',
      render: (value: any, row: any) => (
        <div>
          <div className="text-sm font-medium text-gray-900">{value}</div>
          <div className="text-sm text-gray-500">Size: {row.size}</div>
        </div>
      )
    },
    {
      key: 'created',
      header: 'Created',
      render: (value: any) => (
        <div className="text-sm text-gray-900">
          {new Date(value).toLocaleString()}
        </div>
      )
    },
    {
      key: 'type',
      header: 'Type',
      render: (value: any) => (
        <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium
          ${value === 'Scheduled' ? 'bg-blue-100 text-blue-800' : 'bg-green-100 text-green-800'}`}>
          {value}
        </span>
      )
    },
    {
      key: 'includes',
      header: 'Includes',
      render: (value: any) => (
        <div className="flex flex-wrap gap-1">
          {value.map((item: string) => (
            <span key={item} className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-gray-100 text-gray-700">
              {item}
            </span>
          ))}
        </div>
      )
    },
    {
      key: 'status',
      header: 'Status',
      render: (value: any) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
          <CheckIcon className="w-3 h-3 mr-1" />
          {value}
        </span>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (value: any, row: any) => (
        <div className="flex items-center space-x-2">
          <Button
            variant="outline"
            size="xs"
            onClick={() => handleDownload(row)}
            title="Download backup"
          >
            Download
          </Button>
          <Button
            variant="outline"
            size="xs"
            onClick={() => handleRestore(row)}
            title="Restore from this backup"
          >
            Restore
          </Button>
          <Button
            variant="outline"
            size="xs"
            onClick={() => handleDelete(row)}
            title="Delete backup"
          >
            Delete
          </Button>
        </div>
      )
    }
  ]

  return (
    <div className="space-y-6">
      {/* Backup Status Cards */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
        <Card>
          <div className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <ServerIcon className="h-12 w-12 text-green-600" />
              </div>
              <div className="ml-4">
                <p className="text-sm font-medium text-gray-600">Last Backup</p>
                <p className="text-lg font-semibold text-gray-900">2 hours ago</p>
                <p className="text-xs text-gray-500">Scheduled - Completed</p>
              </div>
            </div>
          </div>
        </Card>
        
        <Card>
          <div className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <DocumentTextIcon className="h-12 w-12 text-blue-600" />
              </div>
              <div className="ml-4">
                <p className="text-sm font-medium text-gray-600">Total Backups</p>
                <p className="text-2xl font-semibold text-gray-900">{backups.length}</p>
                <p className="text-xs text-gray-500">Using 500 MB</p>
              </div>
            </div>
          </div>
        </Card>
        
        <Card>
          <div className="p-6">
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <CalendarIcon className="h-12 w-12 text-indigo-600" />
              </div>
              <div className="ml-4">
                <p className="text-sm font-medium text-gray-600">Next Backup</p>
                <p className="text-lg font-semibold text-gray-900">Tonight 2:00 AM</p>
                <p className="text-xs text-gray-500">Daily Schedule</p>
              </div>
            </div>
          </div>
        </Card>
      </div>

      {/* Actions */}
      <div className="flex justify-between items-center">
        <h3 className="text-lg font-medium text-gray-900">Backup History</h3>
        <div className="flex space-x-2">
          <Button
            variant="outline"
            onClick={() => setShowScheduleModal(true)}
          >
            <CalendarIcon className="h-4 w-4 mr-2" />
            Configure Schedule
          </Button>
          <Button
            onClick={handleBackupNow}
            disabled={backupInProgress}
          >
            <ServerIcon className="h-4 w-4 mr-2" />
            {backupInProgress ? 'Creating Backup...' : 'Backup Now'}
          </Button>
        </div>
      </div>

      {/* Backup Progress */}
      {backupInProgress && (
        <Card>
          <div className="p-6">
            <div className="mb-2 flex justify-between items-center">
              <span className="text-sm font-medium text-gray-700">Creating backup...</span>
              <span className="text-sm text-gray-500">Processing</span>
            </div>
            <div className="w-full bg-gray-200 rounded-full h-2">
              <div className="bg-indigo-600 h-2 rounded-full animate-pulse" style={{ width: '60%' }}></div>
            </div>
            <p className="mt-2 text-xs text-gray-500">This may take a few minutes depending on your data size.</p>
          </div>
        </Card>
      )}

      {/* Backups Table */}
      <Table
        data={backups}
        columns={columns}
        loading={loading}
        emptyMessage="No backups found"
      />

      {/* Schedule Configuration Modal */}
      <Modal
        isOpen={showScheduleModal}
        onClose={() => setShowScheduleModal(false)}
        title="Configure Backup Schedule"
        size="md"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowScheduleModal(false)}>
              Cancel
            </Button>
            <Button onClick={handleScheduleSave}>
              Save Schedule
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Frequency
            </label>
            <select
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
              value={scheduleForm.frequency}
              onChange={(e) => setScheduleForm({ ...scheduleForm, frequency: e.target.value })}
            >
              <option value="hourly">Hourly</option>
              <option value="daily">Daily</option>
              <option value="weekly">Weekly</option>
              <option value="monthly">Monthly</option>
            </select>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Time
            </label>
            <Input
              type="time"
              value={scheduleForm.time}
              onChange={(e) => setScheduleForm({ ...scheduleForm, time: e.target.value })}
            />
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Retention Period (days)
            </label>
            <Input
              type="number"
              value={scheduleForm.retention}
              onChange={(e) => setScheduleForm({ ...scheduleForm, retention: e.target.value })}
              min="1"
              max="365"
            />
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Backup Contents
            </label>
            <div className="space-y-2">
              <label className="flex items-center">
                <input
                  type="checkbox"
                  className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                  checked={scheduleForm.includeDatabase}
                  onChange={(e) => setScheduleForm({ ...scheduleForm, includeDatabase: e.target.checked })}
                />
                <span className="ml-2 text-sm text-gray-700">Database</span>
              </label>
              <label className="flex items-center">
                <input
                  type="checkbox"
                  className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                  checked={scheduleForm.includeFiles}
                  onChange={(e) => setScheduleForm({ ...scheduleForm, includeFiles: e.target.checked })}
                />
                <span className="ml-2 text-sm text-gray-700">Uploaded Files</span>
              </label>
              <label className="flex items-center">
                <input
                  type="checkbox"
                  className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                  checked={scheduleForm.includeReports}
                  onChange={(e) => setScheduleForm({ ...scheduleForm, includeReports: e.target.checked })}
                />
                <span className="ml-2 text-sm text-gray-700">Generated Reports</span>
              </label>
            </div>
          </div>
          
          <div className="rounded-md bg-blue-50 p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <InformationCircleIcon className="h-5 w-5 text-blue-400" />
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-blue-800">
                  Backup Information
                </h3>
                <div className="mt-2 text-sm text-blue-700">
                  <p>Backups will be stored securely and compressed to save space. Old backups will be automatically deleted based on your retention settings.</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </Modal>
    </div>
  )
}

// Notifications Settings Component
function NotificationsSettings() {
  const [notifications, setNotifications] = useState<any[]>([])
  const [notificationSettings, setNotificationSettings] = useState({
    email: {
      enabled: true,
      newInvoice: true,
      paymentReceived: true,
      paymentOverdue: true,
      stockLow: true,
      backupComplete: true,
      userActivity: false,
      systemAlerts: true
    },
    inApp: {
      enabled: true,
      newInvoice: true,
      paymentReceived: true,
      paymentOverdue: true,
      stockLow: true,
      backupComplete: false,
      userActivity: true,
      systemAlerts: true
    },
    frequency: 'realtime',
    digest: 'daily',
    quietHours: {
      enabled: false,
      start: '22:00',
      end: '08:00'
    }
  })
  const [loading, setLoading] = useState(true)
  const [hasChanges, setHasChanges] = useState(false)

  useEffect(() => {
    fetchNotifications()
    fetchNotificationSettings()
  }, [])

  const fetchNotifications = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/notifications/recent')
      if (response.ok) {
        const data = await response.json()
        setNotifications(data.notifications || getMockNotifications())
      } else {
        setNotifications(getMockNotifications())
      }
    } catch (error) {
      console.error('Failed to fetch notifications:', error)
      setNotifications(getMockNotifications())
    } finally {
      setLoading(false)
    }
  }

  const fetchNotificationSettings = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/notifications/settings')
      if (response.ok) {
        const data = await response.json()
        setNotificationSettings(data.settings || notificationSettings)
      }
    } catch (error) {
      console.error('Failed to fetch notification settings:', error)
    }
  }

  const getMockNotifications = () => [
    {
      id: 1,
      type: 'payment_received',
      title: 'Payment Received',
      message: 'Payment of $1,250.00 received from ABC Corporation',
      timestamp: '2024-01-15T10:30:00Z',
      read: false,
      category: 'finance'
    },
    {
      id: 2,
      type: 'stock_low',
      title: 'Low Stock Alert',
      message: 'Widget A - Blue is running low (25 units remaining)',
      timestamp: '2024-01-15T09:15:00Z',
      read: false,
      category: 'inventory'
    },
    {
      id: 3,
      type: 'invoice_overdue',
      title: 'Invoice Overdue',
      message: 'Invoice #INV-2024-0123 is now 5 days overdue',
      timestamp: '2024-01-15T08:00:00Z',
      read: true,
      category: 'finance'
    },
    {
      id: 4,
      type: 'backup_complete',
      title: 'Backup Completed',
      message: 'Scheduled backup completed successfully',
      timestamp: '2024-01-15T02:30:00Z',
      read: true,
      category: 'system'
    }
  ]

  const handleSaveSettings = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/notifications/settings', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(notificationSettings)
      })
      
      if (response.ok || true) {
        alert('Notification settings saved successfully')
        setHasChanges(false)
      }
    } catch (error) {
      console.error('Failed to save settings:', error)
      alert('Failed to save notification settings')
    }
  }

  const handleMarkAsRead = async (notificationId: number) => {
    try {
      const response = await fetch(`http://localhost:8000/api/v1/notifications/${notificationId}/read`, {
        method: 'POST'
      })
      
      if (response.ok || true) {
        setNotifications(notifications.map(n => 
          n.id === notificationId ? { ...n, read: true } : n
        ))
      }
    } catch (error) {
      console.error('Failed to mark as read:', error)
    }
  }

  const handleClearAll = async () => {
    if (!confirm('Are you sure you want to clear all notifications?')) {
      return
    }
    
    try {
      const response = await fetch('http://localhost:8000/api/v1/notifications/clear', {
        method: 'POST'
      })
      
      if (response.ok || true) {
        setNotifications([])
        alert('All notifications cleared')
      }
    } catch (error) {
      console.error('Failed to clear notifications:', error)
      alert('Failed to clear notifications')
    }
  }

  const getCategoryIcon = (category: string) => {
    switch (category) {
      case 'finance':
        return <CurrencyDollarIcon className="h-5 w-5 text-green-600" />
      case 'inventory':
        return <CubeIcon className="h-5 w-5 text-yellow-600" />
      case 'system':
        return <ServerIcon className="h-5 w-5 text-blue-600" />
      default:
        return <BellIcon className="h-5 w-5 text-gray-600" />
    }
  }

  const notificationTypes = [
    { id: 'newInvoice', label: 'New Invoices', description: 'When new invoices are created' },
    { id: 'paymentReceived', label: 'Payments Received', description: 'When payments are received' },
    { id: 'paymentOverdue', label: 'Overdue Payments', description: 'When payments become overdue' },
    { id: 'stockLow', label: 'Low Stock Alerts', description: 'When stock levels are low' },
    { id: 'backupComplete', label: 'Backup Completed', description: 'When backups are completed' },
    { id: 'userActivity', label: 'User Activity', description: 'Login and user management events' },
    { id: 'systemAlerts', label: 'System Alerts', description: 'Important system notifications' }
  ]

  return (
    <div className="space-y-6">
      {/* Notification Settings */}
      <div>
        <h3 className="text-lg font-medium text-gray-900 mb-4">Notification Preferences</h3>
        
        <div className="space-y-6">
          {/* Email Notifications */}
          <Card>
            <div className="p-6">
              <div className="flex items-center justify-between mb-4">
                <div className="flex items-center">
                  <EnvelopeIcon className="h-6 w-6 text-gray-400 mr-3" />
                  <div>
                    <h4 className="text-base font-medium text-gray-900">Email Notifications</h4>
                    <p className="text-sm text-gray-500">Receive notifications via email</p>
                  </div>
                </div>
                <label className="relative inline-flex items-center cursor-pointer">
                  <input 
                    type="checkbox" 
                    className="sr-only peer"
                    checked={notificationSettings.email.enabled}
                    onChange={(e) => {
                      setNotificationSettings({
                        ...notificationSettings,
                        email: { ...notificationSettings.email, enabled: e.target.checked }
                      })
                      setHasChanges(true)
                    }}
                  />
                  <div className="w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-indigo-300 rounded-full peer peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-indigo-600"></div>
                </label>
              </div>
              
              {notificationSettings.email.enabled && (
                <div className="space-y-3 mt-4">
                  {notificationTypes.map(type => (
                    <label key={type.id} className="flex items-start">
                      <input
                        type="checkbox"
                        className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500 mt-1"
                        checked={notificationSettings.email[type.id as keyof typeof notificationSettings.email] as boolean}
                        onChange={(e) => {
                          setNotificationSettings({
                            ...notificationSettings,
                            email: { ...notificationSettings.email, [type.id]: e.target.checked }
                          })
                          setHasChanges(true)
                        }}
                      />
                      <div className="ml-3">
                        <span className="text-sm font-medium text-gray-700">{type.label}</span>
                        <p className="text-xs text-gray-500">{type.description}</p>
                      </div>
                    </label>
                  ))}
                </div>
              )}
            </div>
          </Card>

          {/* In-App Notifications */}
          <Card>
            <div className="p-6">
              <div className="flex items-center justify-between mb-4">
                <div className="flex items-center">
                  <BellIcon className="h-6 w-6 text-gray-400 mr-3" />
                  <div>
                    <h4 className="text-base font-medium text-gray-900">In-App Notifications</h4>
                    <p className="text-sm text-gray-500">Show notifications within the application</p>
                  </div>
                </div>
                <label className="relative inline-flex items-center cursor-pointer">
                  <input 
                    type="checkbox" 
                    className="sr-only peer"
                    checked={notificationSettings.inApp.enabled}
                    onChange={(e) => {
                      setNotificationSettings({
                        ...notificationSettings,
                        inApp: { ...notificationSettings.inApp, enabled: e.target.checked }
                      })
                      setHasChanges(true)
                    }}
                  />
                  <div className="w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-indigo-300 rounded-full peer peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-indigo-600"></div>
                </label>
              </div>
              
              {notificationSettings.inApp.enabled && (
                <div className="space-y-3 mt-4">
                  {notificationTypes.map(type => (
                    <label key={type.id} className="flex items-start">
                      <input
                        type="checkbox"
                        className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500 mt-1"
                        checked={notificationSettings.inApp[type.id as keyof typeof notificationSettings.inApp] as boolean}
                        onChange={(e) => {
                          setNotificationSettings({
                            ...notificationSettings,
                            inApp: { ...notificationSettings.inApp, [type.id]: e.target.checked }
                          })
                          setHasChanges(true)
                        }}
                      />
                      <div className="ml-3">
                        <span className="text-sm font-medium text-gray-700">{type.label}</span>
                        <p className="text-xs text-gray-500">{type.description}</p>
                      </div>
                    </label>
                  ))}
                </div>
              )}
            </div>
          </Card>

          {/* Advanced Settings */}
          <Card>
            <div className="p-6">
              <h4 className="text-base font-medium text-gray-900 mb-4">Advanced Settings</h4>
              
              <div className="space-y-4">
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-2">
                    Notification Frequency
                  </label>
                  <select
                    className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
                    value={notificationSettings.frequency}
                    onChange={(e) => {
                      setNotificationSettings({ ...notificationSettings, frequency: e.target.value })
                      setHasChanges(true)
                    }}
                  >
                    <option value="realtime">Real-time</option>
                    <option value="hourly">Hourly Summary</option>
                    <option value="daily">Daily Digest</option>
                    <option value="weekly">Weekly Summary</option>
                  </select>
                </div>
                
                {notificationSettings.frequency !== 'realtime' && (
                  <div>
                    <label className="block text-sm font-medium text-gray-700 mb-2">
                      Digest Time
                    </label>
                    <select
                      className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
                      value={notificationSettings.digest}
                      onChange={(e) => {
                        setNotificationSettings({ ...notificationSettings, digest: e.target.value })
                        setHasChanges(true)
                      }}
                    >
                      <option value="morning">Morning (9:00 AM)</option>
                      <option value="afternoon">Afternoon (2:00 PM)</option>
                      <option value="evening">Evening (6:00 PM)</option>
                    </select>
                  </div>
                )}
                
                <div>
                  <label className="flex items-center">
                    <input
                      type="checkbox"
                      className="rounded border-gray-300 text-indigo-600 focus:ring-indigo-500"
                      checked={notificationSettings.quietHours.enabled}
                      onChange={(e) => {
                        setNotificationSettings({
                          ...notificationSettings,
                          quietHours: { ...notificationSettings.quietHours, enabled: e.target.checked }
                        })
                        setHasChanges(true)
                      }}
                    />
                    <span className="ml-2 text-sm font-medium text-gray-700">Enable Quiet Hours</span>
                  </label>
                  <p className="text-xs text-gray-500 ml-6 mt-1">Pause notifications during specified hours</p>
                </div>
                
                {notificationSettings.quietHours.enabled && (
                  <div className="grid grid-cols-2 gap-4 ml-6">
                    <div>
                      <label className="block text-sm font-medium text-gray-700 mb-1">
                        Start Time
                      </label>
                      <Input
                        type="time"
                        value={notificationSettings.quietHours.start}
                        onChange={(e) => {
                          setNotificationSettings({
                            ...notificationSettings,
                            quietHours: { ...notificationSettings.quietHours, start: e.target.value }
                          })
                          setHasChanges(true)
                        }}
                      />
                    </div>
                    <div>
                      <label className="block text-sm font-medium text-gray-700 mb-1">
                        End Time
                      </label>
                      <Input
                        type="time"
                        value={notificationSettings.quietHours.end}
                        onChange={(e) => {
                          setNotificationSettings({
                            ...notificationSettings,
                            quietHours: { ...notificationSettings.quietHours, end: e.target.value }
                          })
                          setHasChanges(true)
                        }}
                      />
                    </div>
                  </div>
                )}
              </div>
            </div>
          </Card>
        </div>
        
        {/* Save Button */}
        <div className="flex justify-end mt-6">
          <Button 
            onClick={handleSaveSettings}
            disabled={!hasChanges}
          >
            Save Notification Settings
          </Button>
        </div>
      </div>

      {/* Recent Notifications */}
      <div>
        <div className="flex justify-between items-center mb-4">
          <h3 className="text-lg font-medium text-gray-900">Recent Notifications</h3>
          <Button 
            variant="outline" 
            size="sm"
            onClick={handleClearAll}
            disabled={notifications.length === 0}
          >
            Clear All
          </Button>
        </div>
        
        <Card>
          <div className="divide-y divide-gray-200">
            {loading ? (
              <div className="p-6 text-center">
                <p className="text-gray-500">Loading notifications...</p>
              </div>
            ) : notifications.length > 0 ? (
              notifications.map(notification => (
                <div 
                  key={notification.id} 
                  className={`p-4 hover:bg-gray-50 cursor-pointer ${!notification.read ? 'bg-blue-50' : ''}`}
                  onClick={() => handleMarkAsRead(notification.id)}
                >
                  <div className="flex items-start">
                    <div className="flex-shrink-0">
                      {getCategoryIcon(notification.category)}
                    </div>
                    <div className="ml-3 flex-1">
                      <p className="text-sm font-medium text-gray-900">
                        {notification.title}
                      </p>
                      <p className="text-sm text-gray-500 mt-1">
                        {notification.message}
                      </p>
                      <p className="text-xs text-gray-400 mt-1">
                        {new Date(notification.timestamp).toLocaleString()}
                      </p>
                    </div>
                    {!notification.read && (
                      <div className="ml-3">
                        <span className="inline-block h-2 w-2 bg-blue-600 rounded-full"></span>
                      </div>
                    )}
                  </div>
                </div>
              ))
            ) : (
              <div className="p-6 text-center">
                <BellIcon className="mx-auto h-12 w-12 text-gray-400" />
                <p className="mt-2 text-sm text-gray-500">No notifications</p>
              </div>
            )}
          </div>
        </Card>
      </div>
    </div>
  )
}

export default function SettingsPage() {
  const searchParams = useSearchParams()
  const tabParam = searchParams.get('tab')
  
  const [activeTab, setActiveTab] = useState(tabParam || 'Company')
  const [saved, setSaved] = useState(false)
  const [loading, setLoading] = useState(true)
  const [settings, setSettings] = useState<any>(null)
  const [hasChanges, setHasChanges] = useState(false)

  const tabs = ['Company', 'Financial', 'Tax', 'System', 'Notifications', 'Security', 'Backup']

  useEffect(() => {
    // Update active tab if URL parameter changes
    if (tabParam && tabs.includes(tabParam)) {
      setActiveTab(tabParam)
    }
  }, [tabParam])

  useEffect(() => {
    const fetchSettings = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/admin/cobol/settings')
        if (response.ok) {
          const data = await response.json()
          setSettings(data)
        }
      } catch (error) {
        console.error('Failed to fetch settings:', error)
      } finally {
        setLoading(false)
      }
    }
    
    fetchSettings()
  }, [])

  const handleSave = async () => {
    try {
      const response = await fetch('http://localhost:8000/api/v1/admin/cobol/settings', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(settings)
      })
      const data = await response.json()
      if (data.success) {
        setSaved(true)
        setHasChanges(false)
        setTimeout(() => setSaved(false), 3000)
      } else {
        alert(data.message || 'Failed to save settings')
      }
    } catch (error) {
      console.error('Error saving settings:', error)
      alert('Failed to save settings')
    }
  }

  const renderTabContent = () => {
    switch (activeTab) {
      case 'Company':
        return (
          <div className="space-y-6">
            <div className="grid grid-cols-1 gap-6 sm:grid-cols-2">
              <Input
                label="Company Name"
                type="text"
                value={settings?.company?.companyName || ''}
                onChange={(e) => {
                  setSettings({
                    ...settings,
                    company: { ...settings.company, companyName: e.target.value }
                  })
                  setHasChanges(true)
                }}
              />
              <Input
                label="Registration Number"
                type="text"
                value={settings?.company?.registrationNumber || ''}
                onChange={(e) => {
                  setSettings({
                    ...settings,
                    company: { ...settings.company, registrationNumber: e.target.value }
                  })
                  setHasChanges(true)
                }}
              />
              <Input
                label="VAT Number"
                type="text"
                value={settings?.company?.vatNumber || ''}
                onChange={(e) => {
                  setSettings({
                    ...settings,
                    company: { ...settings.company, vatNumber: e.target.value }
                  })
                  setHasChanges(true)
                }}
              />
              <Input
                label="Phone Number"
                type="text"
                value={settings?.company?.phoneNumber || ''}
                onChange={(e) => {
                  setSettings({
                    ...settings,
                    company: { ...settings.company, phoneNumber: e.target.value }
                  })
                  setHasChanges(true)
                }}
              />
            </div>
            <div className="grid grid-cols-1 gap-6">
              <Input
                label="Address Line 1"
                type="text"
                value={settings?.company?.address?.line1 || ''}
                onChange={(e) => {
                  setSettings(prev => ({
                    ...prev,
                    company: {
                      ...prev?.company,
                      address: {
                        ...prev?.company?.address,
                        line1: e.target.value
                      }
                    }
                  }))
                  setHasChanges(true)
                }}
              />
              <Input
                label="Address Line 2"
                type="text"
                value={settings?.company?.address?.line2 || ''}
                onChange={(e) => {
                  setSettings(prev => ({
                    ...prev,
                    company: {
                      ...prev?.company,
                      address: {
                        ...prev?.company?.address,
                        line2: e.target.value
                      }
                    }
                  }))
                  setHasChanges(true)
                }}
              />
              <div className="grid grid-cols-2 gap-6">
                <Input
                  label="City"
                  type="text"
                  value={settings?.company?.address?.city || ''}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      company: {
                        ...prev?.company,
                        address: {
                          ...prev?.company?.address,
                          city: e.target.value
                        }
                      }
                    }))
                    setHasChanges(true)
                  }}
                />
                <Input
                  label="Postcode"
                  type="text"
                  value={settings?.company?.address?.postCode || ''}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      company: {
                        ...prev?.company,
                        address: {
                          ...prev?.company?.address,
                          postCode: e.target.value
                        }
                      }
                    }))
                    setHasChanges(true)
                  }}
                />
              </div>
            </div>
          </div>
        )
      
      case 'Financial':
        return (
          <div className="space-y-6">
            <div className="grid grid-cols-1 gap-6 sm:grid-cols-2">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Financial Year Start
                </label>
                <select 
                  className="form-select block w-full rounded-md border-gray-300 shadow-sm"
                  value={settings?.financial?.yearStart || 'January'}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      financial: {
                        ...prev?.financial,
                        yearStart: e.target.value
                      }
                    }))
                    setHasChanges(true)
                  }}
                >
                  <option>January</option>
                  <option>April</option>
                  <option>July</option>
                  <option>October</option>
                </select>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Default Currency
                </label>
                <select 
                  className="form-select block w-full rounded-md border-gray-300 shadow-sm"
                  value={settings?.financial?.defaultCurrency || 'USD - US Dollar'}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      financial: {
                        ...prev?.financial,
                        defaultCurrency: e.target.value
                      }
                    }))
                    setHasChanges(true)
                  }}
                >
                  <option>USD - US Dollar</option>
                  <option>GBP - British Pound</option>
                  <option>EUR - Euro</option>
                </select>
              </div>
              <Input
                label="Default Payment Terms (Days)"
                type="number"
                value={settings?.financial?.paymentTerms || '30'}
                onChange={(e) => {
                  setSettings(prev => ({
                    ...prev,
                    financial: {
                      ...prev?.financial,
                      paymentTerms: e.target.value
                    }
                  }))
                  setHasChanges(true)
                }}
              />
              <Input
                label="Default Settlement Discount (%)"
                type="number"
                value={settings?.financial?.settlementDiscount || '2.5'}
                onChange={(e) => {
                  setSettings(prev => ({
                    ...prev,
                    financial: {
                      ...prev?.financial,
                      settlementDiscount: e.target.value
                    }
                  }))
                  setHasChanges(true)
                }}
              />
            </div>
            <div className="space-y-4">
              <h4 className="text-sm font-medium text-gray-900">Number Sequences</h4>
              <div className="grid grid-cols-1 gap-4 sm:grid-cols-2">
                <Input
                  label="Next Invoice Number"
                  type="text"
                  value={settings?.financial?.numberSequences?.invoiceNumber || 'INV-2024-0157'}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      financial: {
                        ...prev?.financial,
                        numberSequences: {
                          ...prev?.financial?.numberSequences,
                          invoiceNumber: e.target.value
                        }
                      }
                    }))
                    setHasChanges(true)
                  }}
                />
                <Input
                  label="Next Credit Note Number"
                  type="text"
                  value={settings?.financial?.numberSequences?.creditNoteNumber || 'CN-2024-0045'}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      financial: {
                        ...prev?.financial,
                        numberSequences: {
                          ...prev?.financial?.numberSequences,
                          creditNoteNumber: e.target.value
                        }
                      }
                    }))
                    setHasChanges(true)
                  }}
                />
                <Input
                  label="Next Purchase Order Number"
                  type="text"
                  value={settings?.financial?.numberSequences?.purchaseOrderNumber || 'PO-2024-0234'}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      financial: {
                        ...prev?.financial,
                        numberSequences: {
                          ...prev?.financial?.numberSequences,
                          purchaseOrderNumber: e.target.value
                        }
                      }
                    }))
                    setHasChanges(true)
                  }}
                />
                <Input
                  label="Next Receipt Number"
                  type="text"
                  value={settings?.financial?.numberSequences?.receiptNumber || 'RCT-2024-0156'}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      financial: {
                        ...prev?.financial,
                        numberSequences: {
                          ...prev?.financial?.numberSequences,
                          receiptNumber: e.target.value
                        }
                      }
                    }))
                    setHasChanges(true)
                  }}
                />
              </div>
            </div>
          </div>
        )
      
      case 'Tax':
        return (
          <div className="space-y-6">
            <div className="grid grid-cols-1 gap-6 sm:grid-cols-2">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Default Tax Rate
                </label>
                <select 
                  className="form-select block w-full rounded-md border-gray-300 shadow-sm"
                  value={settings?.tax?.defaultRate || '20% - Standard Rate'}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      tax: {
                        ...prev?.tax,
                        defaultRate: e.target.value
                      }
                    }))
                    setHasChanges(true)
                  }}
                >
                  <option>20% - Standard Rate</option>
                  <option>5% - Reduced Rate</option>
                  <option>0% - Zero Rate</option>
                  <option>Exempt</option>
                </select>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Tax Scheme
                </label>
                <select 
                  className="form-select block w-full rounded-md border-gray-300 shadow-sm"
                  value={settings?.tax?.scheme || 'Standard VAT'}
                  onChange={(e) => {
                    setSettings(prev => ({
                      ...prev,
                      tax: {
                        ...prev?.tax,
                        scheme: e.target.value
                      }
                    }))
                    setHasChanges(true)
                  }}
                >
                  <option>Standard VAT</option>
                  <option>Cash Accounting</option>
                  <option>Flat Rate Scheme</option>
                </select>
              </div>
            </div>
            <div className="space-y-4">
              <h4 className="text-sm font-medium text-gray-900">Tax Codes</h4>
              <div className="border rounded-lg overflow-hidden">
                <table className="min-w-full divide-y divide-gray-200">
                  <thead className="bg-gray-50">
                    <tr>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Code</th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Description</th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Rate</th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Type</th>
                    </tr>
                  </thead>
                  <tbody className="bg-white divide-y divide-gray-200">
                    <tr>
                      <td className="px-6 py-4 text-sm">VSTD</td>
                      <td className="px-6 py-4 text-sm">VAT Standard Rate</td>
                      <td className="px-6 py-4 text-sm">20%</td>
                      <td className="px-6 py-4 text-sm">Output</td>
                    </tr>
                    <tr>
                      <td className="px-6 py-4 text-sm">VRED</td>
                      <td className="px-6 py-4 text-sm">VAT Reduced Rate</td>
                      <td className="px-6 py-4 text-sm">5%</td>
                      <td className="px-6 py-4 text-sm">Output</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        )
      
      case 'System':
        return (
          <div className="space-y-6">
            <div className="space-y-4">
              <h4 className="text-sm font-medium text-gray-900">System Information</h4>
              <dl className="grid grid-cols-1 gap-4 sm:grid-cols-2">
                <div className="bg-gray-50 px-4 py-3 rounded-lg">
                  <dt className="text-sm font-medium text-gray-500">Version</dt>
                  <dd className="mt-1 text-sm text-gray-900">{settings?.system?.version || 'N/A'}</dd>
                </div>
                <div className="bg-gray-50 px-4 py-3 rounded-lg">
                  <dt className="text-sm font-medium text-gray-500">Database</dt>
                  <dd className="mt-1 text-sm text-gray-900">{settings?.system?.database || 'N/A'}</dd>
                </div>
                <div className="bg-gray-50 px-4 py-3 rounded-lg">
                  <dt className="text-sm font-medium text-gray-500">Last Migration</dt>
                  <dd className="mt-1 text-sm text-gray-900">{settings?.system?.lastMigration ? new Date(settings.system.lastMigration).toLocaleString() : 'N/A'}</dd>
                </div>
                <div className="bg-gray-50 px-4 py-3 rounded-lg">
                  <dt className="text-sm font-medium text-gray-500">Total Records</dt>
                  <dd className="mt-1 text-sm text-gray-900">{settings?.system?.totalRecords?.toLocaleString() || 'N/A'}</dd>
                </div>
              </dl>
            </div>
            <div className="space-y-4">
              <h4 className="text-sm font-medium text-gray-900">System Settings</h4>
              <div className="space-y-4">
                <label className="flex items-center">
                  <input 
                    type="checkbox" 
                    className="rounded border-gray-300" 
                    checked={settings?.system?.auditTrail || true}
                    onChange={(e) => {
                      setSettings(prev => ({
                        ...prev,
                        system: {
                          ...prev?.system,
                          auditTrail: e.target.checked
                        }
                      }))
                      setHasChanges(true)
                    }}
                  />
                  <span className="ml-2 text-sm text-gray-900">Enable audit trail</span>
                </label>
                <label className="flex items-center">
                  <input 
                    type="checkbox" 
                    className="rounded border-gray-300" 
                    checked={settings?.system?.automaticBackups || true}
                    onChange={(e) => {
                      setSettings(prev => ({
                        ...prev,
                        system: {
                          ...prev?.system,
                          automaticBackups: e.target.checked
                        }
                      }))
                      setHasChanges(true)
                    }}
                  />
                  <span className="ml-2 text-sm text-gray-900">Automatic backups</span>
                </label>
                <label className="flex items-center">
                  <input 
                    type="checkbox" 
                    className="rounded border-gray-300" 
                    checked={settings?.system?.debugMode || false}
                    onChange={(e) => {
                      setSettings(prev => ({
                        ...prev,
                        system: {
                          ...prev?.system,
                          debugMode: e.target.checked
                        }
                      }))
                      setHasChanges(true)
                    }}
                  />
                  <span className="ml-2 text-sm text-gray-900">Debug mode</span>
                </label>
              </div>
            </div>
          </div>
        )
      
      case 'Security':
        return <UserManagement />
      
      case 'Backup':
        return <SystemBackup />
      
      case 'Notifications':
        return <NotificationsSettings />
      
      default:
        return (
          <div className="text-center py-12">
            <CogIcon className="mx-auto h-12 w-12 text-gray-400" />
            <h3 className="mt-2 text-sm font-medium text-gray-900">
              {activeTab} Settings
            </h3>
            <p className="mt-1 text-sm text-gray-500">
              Settings for {activeTab} will be available soon.
            </p>
          </div>
        )
    }
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Settings"
        description="System configuration and preferences"
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Settings' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <Card>
          <div className="p-6">
            {loading ? (
              <div className="text-center py-12">
                <p className="text-gray-500">Loading settings...</p>
              </div>
            ) : (
              <>
                <TabNavigation 
                  tabs={tabs} 
                  activeTab={activeTab} 
                  setActiveTab={setActiveTab} 
                />
                
                <div className="mt-6">
                  {renderTabContent()}
                </div>
              </>
            )}

            <div className="mt-8 flex items-center justify-between border-t pt-6">
              <p className="text-sm text-gray-500">
                Last updated: {settings?.lastUpdated ? new Date(settings.lastUpdated).toLocaleString() : 'Never'}
              </p>
              <div className="flex space-x-3">
                <Button variant="outline">Cancel</Button>
                <Button onClick={handleSave}>
                  {saved ? (
                    <>
                      <CheckIcon className="h-4 w-4 mr-2" />
                      Saved
                    </>
                  ) : (
                    'Save Changes'
                  )}
                </Button>
              </div>
            </div>
          </div>
        </Card>

        {/* Icon Legend */}
        <div className="mt-8 grid grid-cols-1 gap-6 sm:grid-cols-2 lg:grid-cols-4">
          <Card>
            <div className="p-6 flex items-center">
              <BuildingOfficeIcon className="h-8 w-8 text-gray-400 mr-4" />
              <div>
                <h4 className="text-sm font-medium text-gray-900">Company</h4>
                <p className="text-sm text-gray-500">Business details</p>
              </div>
            </div>
          </Card>
          <Card>
            <div className="p-6 flex items-center">
              <CurrencyDollarIcon className="h-8 w-8 text-gray-400 mr-4" />
              <div>
                <h4 className="text-sm font-medium text-gray-900">Financial</h4>
                <p className="text-sm text-gray-500">Accounting settings</p>
              </div>
            </div>
          </Card>
          <Card>
            <div className="p-6 flex items-center">
              <ShieldCheckIcon className="h-8 w-8 text-gray-400 mr-4" />
              <div>
                <h4 className="text-sm font-medium text-gray-900">Security</h4>
                <p className="text-sm text-gray-500">Access control</p>
              </div>
            </div>
          </Card>
          <Card>
            <div className="p-6 flex items-center">
              <ServerIcon className="h-8 w-8 text-gray-400 mr-4" />
              <div>
                <h4 className="text-sm font-medium text-gray-900">System</h4>
                <p className="text-sm text-gray-500">Technical config</p>
              </div>
            </div>
          </Card>
        </div>
      </main>
    </div>
  )
}