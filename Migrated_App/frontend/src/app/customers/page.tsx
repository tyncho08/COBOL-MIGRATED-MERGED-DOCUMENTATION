'use client'

import { useState, useEffect } from 'react'
import { 
  UsersIcon,
  MagnifyingGlassIcon,
  PlusIcon,
  CurrencyDollarIcon,
  CheckCircleIcon,
  ExclamationTriangleIcon,
  XCircleIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import Table from '@/components/UI/Table'
import PageHeader from '@/components/Layout/PageHeader'
import { customerApi, type CustomerSummary } from '@/lib/api'

export default function CustomersPage() {
  const [customers, setCustomers] = useState<CustomerSummary[]>([])
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [searchTerm, setSearchTerm] = useState('')
  const [selectedCustomers, setSelectedCustomers] = useState<string[]>([])

  useEffect(() => {
    const fetchCustomers = async () => {
      try {
        setLoading(true)
        setError(null)
        
        const data = await customerApi.getCustomers(0, 100)
        setCustomers(data)
      } catch (err) {
        console.error('Failed to fetch customers:', err)
        setError(err instanceof Error ? err.message : 'Failed to fetch customers')
      } finally {
        setLoading(false)
      }
    }

    fetchCustomers()
  }, [])

  // Filter customers based on search term
  const filteredCustomers = customers.filter(customer =>
    customer.sales_name.toLowerCase().includes(searchTerm.toLowerCase()) ||
    customer.sales_key.toLowerCase().includes(searchTerm.toLowerCase())
  )

  // Calculate summary stats
  const stats = {
    totalCustomers: customers.length,
    activeCustomers: customers.filter(c => c.is_active).length,
    totalOutstanding: customers.reduce((sum, c) => sum + parseFloat(c.sales_balance), 0),
    totalCreditLimit: customers.reduce((sum, c) => sum + parseFloat(c.sales_credit_limit), 0)
  }

  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-GB', {
      style: 'currency',
      currency: 'GBP'
    }).format(amount)
  }

  const getStatusBadge = (customer: CustomerSummary) => {
    if (!customer.is_active) {
      return (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
          <XCircleIcon className="w-3 h-3 mr-1" />
          Inactive
        </span>
      )
    }
    
    const balance = parseFloat(customer.sales_balance)
    const creditLimit = parseFloat(customer.sales_credit_limit)
    
    if (balance > creditLimit) {
      return (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
          <ExclamationTriangleIcon className="w-3 h-3 mr-1" />
          Over Limit
        </span>
      )
    }
    
    return (
      <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
        <CheckCircleIcon className="w-3 h-3 mr-1" />
        Active
      </span>
    )
  }

  const columns = [
    {
      key: 'sales_key',
      header: 'Customer Code',
      render: (customer: CustomerSummary) => (
        <div className="font-medium text-gray-900">{customer.sales_key}</div>
      )
    },
    {
      key: 'sales_name',
      header: 'Customer Name',
      render: (customer: CustomerSummary) => (
        <div>
          <div className="font-medium text-gray-900">{customer.sales_name}</div>
          <div className="text-sm text-gray-500">Status: {customer.sales_account_status}</div>
        </div>
      )
    },
    {
      key: 'sales_balance',
      header: 'Balance',
      render: (customer: CustomerSummary) => (
        <div className="text-right">
          <div className="font-medium text-gray-900">
            {formatCurrency(parseFloat(customer.sales_balance))}
          </div>
          {parseFloat(customer.sales_balance) > 0 && (
            <div className="text-sm text-red-600">Outstanding</div>
          )}
        </div>
      )
    },
    {
      key: 'sales_credit_limit',
      header: 'Credit Limit',
      render: (customer: CustomerSummary) => (
        <div className="text-right">
          <div className="font-medium text-gray-900">
            {formatCurrency(parseFloat(customer.sales_credit_limit))}
          </div>
          <div className="text-sm text-gray-500">
            Available: {formatCurrency(parseFloat(customer.sales_credit_limit) - parseFloat(customer.sales_balance))}
          </div>
        </div>
      )
    },
    {
      key: 'status',
      header: 'Status',
      render: (customer: CustomerSummary) => getStatusBadge(customer)
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (customer: CustomerSummary) => (
        <div className="flex space-x-2">
          <Button variant="outline" size="sm">
            View
          </Button>
          <Button variant="outline" size="sm">
            Edit
          </Button>
        </div>
      )
    }
  ]

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        Export
      </Button>
      <Button size="sm">
        <PlusIcon className="h-4 w-4" />
        New Customer
      </Button>
    </div>
  )

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Customers"
        description="Customer management and accounts receivable"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Sales', href: '/sales' },
          { label: 'Customers' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          <StatsCard
            title="Total Customers"
            value={stats.totalCustomers.toLocaleString()}
            icon={<UsersIcon className="h-6 w-6" />}
            change={{ 
              value: `${stats.activeCustomers} active`, 
              type: 'neutral' 
            }}
          />
          <StatsCard
            title="Total Outstanding"
            value={formatCurrency(stats.totalOutstanding)}
            icon={<CurrencyDollarIcon className="h-6 w-6" />}
            change={{ 
              value: `${customers.filter(c => parseFloat(c.sales_balance) > 0).length} with balance`, 
              type: 'neutral' 
            }}
          />
          <StatsCard
            title="Total Credit Limit"
            value={formatCurrency(stats.totalCreditLimit)}
            icon={<CurrencyDollarIcon className="h-6 w-6" />}
            change={{ 
              value: formatCurrency(stats.totalCreditLimit - stats.totalOutstanding) + ' available', 
              type: 'increase' 
            }}
          />
          <StatsCard
            title="Over Limit"
            value={customers.filter(c => parseFloat(c.sales_balance) > parseFloat(c.sales_credit_limit)).length}
            icon={<ExclamationTriangleIcon className="h-6 w-6" />}
            change={{ 
              value: customers.filter(c => !c.is_active).length + ' inactive', 
              type: 'decrease' 
            }}
          />
        </div>

        {/* Search and Filters */}
        <Card className="mb-6">
          <div className="p-6">
            <div className="flex items-center space-x-4">
              <div className="flex-1">
                <Input
                  type="text"
                  placeholder="Search customers by name or code..."
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  leftIcon={<MagnifyingGlassIcon className="h-5 w-5" />}
                />
              </div>
              <Button variant="outline">
                Filter
              </Button>
            </div>
          </div>
        </Card>

        {/* Error State */}
        {error && (
          <Card className="mb-6">
            <div className="p-6">
              <div className="rounded-md bg-red-50 p-4">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <XCircleIcon className="h-5 w-5 text-red-400" />
                  </div>
                  <div className="ml-3">
                    <h3 className="text-sm font-medium text-red-800">
                      Error Loading Customers
                    </h3>
                    <div className="mt-2 text-sm text-red-700">
                      <p>{error}</p>
                    </div>
                    <div className="mt-4">
                      <Button 
                        size="sm" 
                        variant="outline"
                        onClick={() => window.location.reload()}
                      >
                        Retry
                      </Button>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </Card>
        )}

        {/* Customer Table */}
        <Card>
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <h3 className="text-lg font-medium text-gray-900">
                Customer List
                {searchTerm && (
                  <span className="text-sm font-normal text-gray-500 ml-2">
                    ({filteredCustomers.length} of {customers.length} customers)
                  </span>
                )}
              </h3>
              <div className="text-sm text-gray-500">
                {loading ? 'Loading...' : `${filteredCustomers.length} customers`}
              </div>
            </div>
          </div>
          
          <Table
            data={filteredCustomers}
            columns={columns}
            loading={loading}
            emptyMessage="No customers found"
            onRowClick={(customer) => {
              // Navigate to customer detail page
              console.log('View customer:', customer.sales_key)
            }}
          />
        </Card>

        {/* Real Data Badge */}
        <div className="mt-8 flex justify-center">
          <div className="inline-flex items-center px-4 py-2 rounded-full text-sm font-medium bg-green-100 text-green-800">
            <CheckCircleIcon className="w-4 h-4 mr-2" />
            ✨ Showing REAL data from PostgreSQL database
          </div>
        </div>
      </main>
    </div>
  )
}