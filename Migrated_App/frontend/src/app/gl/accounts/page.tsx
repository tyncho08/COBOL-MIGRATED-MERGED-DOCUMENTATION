'use client'

import { useState, useEffect } from 'react'
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

interface GLAccount {
  ledger_key: number
  ledger_name: string
  ledger_type: number
  ledger_place: string
  ledger_level: number
  ledger_balance: string
}

export default function GLAccountsPage() {
  const [accounts, setAccounts] = useState<GLAccount[]>([])
  const [loading, setLoading] = useState(true)
  const [searchTerm, setSearchTerm] = useState('')
  const [filterType, setFilterType] = useState('all')

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
      <Button variant="outline" size="sm">
        <FunnelIcon className="h-4 w-4" />
        Export
      </Button>
      <Button size="sm">
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
          <Button variant="outline" size="xs">
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
              <Button variant="outline" size="sm">
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
    </div>
  )
}