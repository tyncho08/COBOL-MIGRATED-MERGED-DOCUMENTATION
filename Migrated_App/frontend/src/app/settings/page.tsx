'use client'

import { useState, useEffect } from 'react'
import { 
  CogIcon,
  UserIcon,
  BuildingOfficeIcon,
  CurrencyDollarIcon,
  DocumentTextIcon,
  BellIcon,
  ShieldCheckIcon,
  ServerIcon,
  CheckIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import PageHeader from '@/components/Layout/PageHeader'

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

export default function SettingsPage() {
  const [activeTab, setActiveTab] = useState('Company')
  const [saved, setSaved] = useState(false)
  const [loading, setLoading] = useState(true)
  const [settings, setSettings] = useState<any>(null)
  const [hasChanges, setHasChanges] = useState(false)

  const tabs = ['Company', 'Financial', 'Tax', 'System', 'Notifications', 'Security', 'Backup']

  useEffect(() => {
    const fetchSettings = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/admin/settings')
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
      const response = await fetch('http://localhost:8000/api/v1/admin/settings', {
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
                defaultValue="123 Business Street"
              />
              <Input
                label="Address Line 2"
                type="text"
                defaultValue="Commercial District"
              />
              <div className="grid grid-cols-2 gap-6">
                <Input
                  label="City"
                  type="text"
                  defaultValue="London"
                />
                <Input
                  label="Postcode"
                  type="text"
                  defaultValue="SW1A 1AA"
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
                <select className="form-select block w-full rounded-md border-gray-300 shadow-sm">
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
                <select className="form-select block w-full rounded-md border-gray-300 shadow-sm">
                  <option>USD - US Dollar</option>
                  <option>GBP - British Pound</option>
                  <option>EUR - Euro</option>
                </select>
              </div>
              <Input
                label="Default Payment Terms (Days)"
                type="number"
                defaultValue="30"
              />
              <Input
                label="Default Settlement Discount (%)"
                type="number"
                defaultValue="2.5"
              />
            </div>
            <div className="space-y-4">
              <h4 className="text-sm font-medium text-gray-900">Number Sequences</h4>
              <div className="grid grid-cols-1 gap-4 sm:grid-cols-2">
                <Input
                  label="Next Invoice Number"
                  type="text"
                  defaultValue="INV-2024-0157"
                />
                <Input
                  label="Next Credit Note Number"
                  type="text"
                  defaultValue="CN-2024-0045"
                />
                <Input
                  label="Next Purchase Order Number"
                  type="text"
                  defaultValue="PO-2024-0234"
                />
                <Input
                  label="Next Receipt Number"
                  type="text"
                  defaultValue="RCT-2024-0156"
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
                <select className="form-select block w-full rounded-md border-gray-300 shadow-sm">
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
                <select className="form-select block w-full rounded-md border-gray-300 shadow-sm">
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
                  <input type="checkbox" className="rounded border-gray-300" defaultChecked />
                  <span className="ml-2 text-sm text-gray-900">Enable audit trail</span>
                </label>
                <label className="flex items-center">
                  <input type="checkbox" className="rounded border-gray-300" defaultChecked />
                  <span className="ml-2 text-sm text-gray-900">Automatic backups</span>
                </label>
                <label className="flex items-center">
                  <input type="checkbox" className="rounded border-gray-300" />
                  <span className="ml-2 text-sm text-gray-900">Debug mode</span>
                </label>
              </div>
            </div>
          </div>
        )
      
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
                Last updated: 2024-02-20 14:30:00
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