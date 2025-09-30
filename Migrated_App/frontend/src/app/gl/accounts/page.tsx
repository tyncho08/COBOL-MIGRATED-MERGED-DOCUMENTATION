'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import { 
  DocumentTextIcon,
  PlusIcon,
  PencilIcon,
  MagnifyingGlassIcon,
  FunnelIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Table from '@/components/UI/Table'
import Input from '@/components/UI/Input'
import { formatCurrency } from '@/lib/utils'
import Modal from '@/components/UI/Modal'
import Select from '@/components/UI/Select'
import { ReportGenerator } from '@/lib/reportGenerator'

interface GLAccount {
  ledger_key: number
  ledger_name: string
  ledger_type: number
  ledger_place: string
  ledger_level: number
  ledger_balance: string
}

export default function GLAccountsPage() {
  const router = useRouter()
  const [accounts, setAccounts] = useState<GLAccount[]>([])
  const [loading, setLoading] = useState(true)
  const [searchTerm, setSearchTerm] = useState('')
  const [filterType, setFilterType] = useState('all')
  const [showNewAccountModal, setShowNewAccountModal] = useState(false)
  const [showEditModal, setShowEditModal] = useState(false)
  const [selectedAccount, setSelectedAccount] = useState<GLAccount | null>(null)
  const [accountForm, setAccountForm] = useState({
    ledger_key: '',
    ledger_name: '',
    ledger_type: '1',
    ledger_level: '4',
    active: true
  })

  useEffect(() => {
    const fetchAccounts = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/gl/accounts')
        if (response.ok) {
          const data = await response.json()
          setAccounts(data.accounts || [])
        } else {
          // Fallback data if API not available
          setAccounts([
            {
              account_code: '10010000',
              account_name: 'Petty Cash',
              account_type: 'Asset',
              balance: 500.00,
              debit_balance: 500.00,
              credit_balance: 0.00,
              level: 4,
              active: true
            },
            {
              account_code: '10020000',
              account_name: 'Bank Current Account',
              account_type: 'Asset',
              balance: 25000.00,
              debit_balance: 25000.00,
              credit_balance: 0.00,
              level: 4,
              active: true
            },
            {
              account_code: '11010000',
              account_name: 'Trade Debtors Control',
              account_type: 'Asset',
              balance: 11200.00,
              debit_balance: 11200.00,
              credit_balance: 0.00,
              level: 4,
              active: true
            }
          ])
        }
      } catch (error) {
        console.error('Failed to fetch GL accounts:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchAccounts()
  }, [])

  const filteredAccounts = accounts.filter(account => {
    const matchesSearch = (account.ledger_key?.toString() || '').toLowerCase().includes(searchTerm.toLowerCase()) ||
                         (account.ledger_name || '').toLowerCase().includes(searchTerm.toLowerCase())
    const matchesFilter = filterType === 'all' || (account.ledger_type?.toString() || '') === filterType
    return matchesSearch && matchesFilter
  })

  const quickActions = (
    <div className="flex space-x-2">
      <Button 
        variant="outline" 
        size="sm"
        onClick={async () => {
          try {
            const data = filteredAccounts.map(acc => ({
              'Account Code': acc.ledger_key,
              'Account Name': acc.ledger_name,
              'Type': acc.ledger_type === 1 ? 'Asset' : 
                      acc.ledger_type === 2 ? 'Liability' :
                      acc.ledger_type === 3 ? 'Capital/Equity' :
                      acc.ledger_type === 4 ? 'Income/Revenue' :
                      acc.ledger_type === 5 ? 'Expense/Cost' : 'Unknown',
              'Balance': parseFloat(acc.ledger_balance || '0')
            }))
            
            ReportGenerator.exportToExcel(data, 'Chart_of_Accounts')
          } catch (error) {
            console.error('Export error:', error)
            alert('Failed to export accounts')
          }
        }}
      >
        <FunnelIcon className="h-4 w-4" />
        Export
      </Button>
      <Button size="sm" onClick={() => setShowNewAccountModal(true)}>
        <PlusIcon className="h-4 w-4" />
        New Account
      </Button>
    </div>
  )

  const columns = [
    {
      key: 'ledger_key',
      header: 'Account Code',
      className: 'w-32 font-mono',
      render: (value: any) => (
        <span className="font-mono text-sm font-medium">{value}</span>
      )
    },
    {
      key: 'ledger_name',
      header: 'Account Name',
      className: 'min-w-[200px]',
      render: (value: any, row: any) => (
        <div>
          <div className="font-medium text-gray-900">{value}</div>
          <div className="text-sm text-gray-500">
            {row.ledger_type === 1 ? 'Asset' : 
             row.ledger_type === 2 ? 'Liability' :
             row.ledger_type === 3 ? 'Capital/Equity' :
             row.ledger_type === 4 ? 'Income/Revenue' :
             row.ledger_type === 5 ? 'Expense/Cost' : 'Unknown'}
          </div>
        </div>
      )
    },
    {
      key: 'ledger_balance',
      header: 'Balance',
      className: 'w-32 text-right',
      render: (value: any, row: any) => (
        <div className="text-right">
          <div className="font-medium text-gray-900">
            {formatCurrency(Math.abs(parseFloat(value || '0')))}
          </div>
          <div className="text-xs text-gray-500">
            {parseFloat(value || '0') >= 0 ? 'DR' : 'CR'}
          </div>
        </div>
      )
    },
    {
      key: 'actions',
      header: 'Actions',
      className: 'w-24',
      render: (value: any, row: any) => (
        <div className="flex space-x-1">
          <Button 
            variant="outline" 
            size="xs"
            onClick={() => {
              setSelectedAccount(row)
              setAccountForm({
                ledger_key: row.ledger_key.toString(),
                ledger_name: row.ledger_name,
                ledger_type: row.ledger_type.toString(),
                ledger_level: row.ledger_level?.toString() || '4',
                active: true
              })
              setShowEditModal(true)
            }}
          >
            <PencilIcon className="h-3 w-3" />
          </Button>
        </div>
      )
    }
  ]

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Chart of Accounts"
        description="Manage your general ledger chart of accounts"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'General Ledger', href: '/gl' },
          { label: 'Chart of Accounts' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Search and Filter */}
        <div className="mb-6 flex flex-col sm:flex-row gap-4">
          <div className="flex-1">
            <div className="relative">
              <MagnifyingGlassIcon className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
              <Input
                type="text"
                placeholder="Search accounts..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="pl-10"
              />
            </div>
          </div>
          <div className="w-full sm:w-48">
            <select
              value={filterType}
              onChange={(e) => setFilterType(e.target.value)}
              className="block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500"
            >
              <option value="all">All Types</option>
              <option value="1">Assets</option>
              <option value="2">Liabilities</option>
              <option value="3">Capital/Equity</option>
              <option value="4">Income/Revenue</option>
              <option value="5">Expenses/Costs</option>
            </select>
          </div>
        </div>

        {/* Accounts Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="text-lg font-medium text-gray-900">General Ledger Accounts</h3>
                <p className="text-sm text-gray-500">
                  {filteredAccounts.length} of {accounts.length} accounts
                </p>
              </div>
              <Button 
                variant="outline" 
                size="sm"
                onClick={() => router.push('/gl/trial-balance')}
              >
                <DocumentTextIcon className="h-4 w-4" />
                Trial Balance
              </Button>
            </div>
          </div>
          <Table
            data={filteredAccounts}
            columns={columns}
            loading={loading}
            emptyMessage="No accounts found matching your criteria"
          />
        </Card>
      </main>

      {/* New Account Modal */}
      <Modal
        isOpen={showNewAccountModal}
        onClose={() => {
          setShowNewAccountModal(false)
          setAccountForm({
            ledger_key: '',
            ledger_name: '',
            ledger_type: '1',
            ledger_level: '4',
            active: true
          })
        }}
        title="New GL Account"
        size="md"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowNewAccountModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              onClick={async () => {
                try {
                  const response = await fetch('http://localhost:8000/api/v1/gl/accounts', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify(accountForm)
                  })
                  
                  const data = await response.json()
                  if (data.success) {
                    alert('Account created successfully!')
                    setShowNewAccountModal(false)
                    window.location.reload()
                  } else {
                    alert(data.message || 'Failed to create account')
                  }
                } catch (error) {
                  console.error('Error creating account:', error)
                  alert('Failed to create account')
                }
              }}
            >
              Create Account
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <Input
            label="Account Code"
            type="text"
            value={accountForm.ledger_key}
            onChange={(e) => setAccountForm({...accountForm, ledger_key: e.target.value})}
            placeholder="10010000"
            required
          />
          <Input
            label="Account Name"
            type="text"
            value={accountForm.ledger_name}
            onChange={(e) => setAccountForm({...accountForm, ledger_name: e.target.value})}
            placeholder="Cash on Hand"
            required
          />
          <Select
            label="Account Type"
            value={accountForm.ledger_type}
            onChange={(e) => setAccountForm({...accountForm, ledger_type: e.target.value})}
            options={[
              { value: '1', label: 'Asset' },
              { value: '2', label: 'Liability' },
              { value: '3', label: 'Capital/Equity' },
              { value: '4', label: 'Income/Revenue' },
              { value: '5', label: 'Expense/Cost' }
            ]}
          />
          <Select
            label="Account Level"
            value={accountForm.ledger_level}
            onChange={(e) => setAccountForm({...accountForm, ledger_level: e.target.value})}
            options={[
              { value: '1', label: 'Level 1 - Header' },
              { value: '2', label: 'Level 2 - Sub-Header' },
              { value: '3', label: 'Level 3 - Detail' },
              { value: '4', label: 'Level 4 - Transaction' }
            ]}
          />
        </div>
      </Modal>

      {/* Edit Account Modal */}
      <Modal
        isOpen={showEditModal}
        onClose={() => {
          setShowEditModal(false)
          setSelectedAccount(null)
        }}
        title="Edit GL Account"
        size="md"
        actions={
          <>
            <Button variant="outline" onClick={() => setShowEditModal(false)}>
              Cancel
            </Button>
            <Button 
              className="ml-2"
              onClick={async () => {
                try {
                  const response = await fetch(`http://localhost:8000/api/v1/gl/accounts/${selectedAccount?.ledger_key}`, {
                    method: 'PUT',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify(accountForm)
                  })
                  
                  const data = await response.json()
                  if (data.success) {
                    alert('Account updated successfully!')
                    setShowEditModal(false)
                    window.location.reload()
                  } else {
                    alert(data.message || 'Failed to update account')
                  }
                } catch (error) {
                  console.error('Error updating account:', error)
                  alert('Failed to update account')
                }
              }}
            >
              Update Account
            </Button>
          </>
        }
      >
        <div className="space-y-4">
          <Input
            label="Account Code"
            type="text"
            value={accountForm.ledger_key}
            disabled
            className="bg-gray-100"
          />
          <Input
            label="Account Name"
            type="text"
            value={accountForm.ledger_name}
            onChange={(e) => setAccountForm({...accountForm, ledger_name: e.target.value})}
            required
          />
          <Select
            label="Account Type"
            value={accountForm.ledger_type}
            onChange={(e) => setAccountForm({...accountForm, ledger_type: e.target.value})}
            options={[
              { value: '1', label: 'Asset' },
              { value: '2', label: 'Liability' },
              { value: '3', label: 'Capital/Equity' },
              { value: '4', label: 'Income/Revenue' },
              { value: '5', label: 'Expense/Cost' }
            ]}
          />
          <Select
            label="Account Level"
            value={accountForm.ledger_level}
            onChange={(e) => setAccountForm({...accountForm, ledger_level: e.target.value})}
            options={[
              { value: '1', label: 'Level 1 - Header' },
              { value: '2', label: 'Level 2 - Sub-Header' },
              { value: '3', label: 'Level 3 - Detail' },
              { value: '4', label: 'Level 4 - Transaction' }
            ]}
          />
        </div>
      </Modal>
    </div>
  )
}